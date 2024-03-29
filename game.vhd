-- Description: breakout game component
-- FSM-based design 

-- Engineers: Daniel Millett & Ben Moran
-- Date: 01/11/2022
-- 
-- 16 x 32-bit game array, using reg32x32(15 downto 0)(31:0)

-- On completion
--    write reg4x32_CSRA(0)(1:0)  = 0b10, i.e, (1) = 1 => FPGA done, (0) = 0 => return control to host. Other CSRA(0) bits unchanged
--
-- Signal dictionary
--  clk					system clock strobe, rising edge active
--  rst	        		assertion (h) asynchronously clears all registers
--  ce                  chip enable, asserted high                 		 
--  go			        Assertion (H) detected in idle state to active threshold function 
--  active (Output)     Default asserted (h), except in idle state

--  reg4x32_CSRA    	4 x 32-bit Control & Status registers, CSRA
--  reg4x32_CSRB      	32-bit Control register, CSRB
--  BRAM_dOut	        Current source memory 256-bit data (not used in this application)

--  wr  (Output)        Asserted to synchronously write to addressed memory
--  add (Output)  	    Addressed memory - 0b00100000 to read BRAM(255:0)
--  datToMem (Output)   32-bit data to addressed memory 

--  functBus            96-bit array of function signals, for use in debug and demonstration of function behaviour 
--  			        Not used in this example

-- Internal Signal dictionary
--  NS, CS                         finite state machine state signals 
--  NS*, CS* 				       next and current state signals 

--    Using integer types for INTERNAL address signals where possible, to make VHDL model more readable

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.arrayPackage.all;

entity game is
    Port ( clk 		     : in STD_LOGIC;   
           rst 		     : in STD_LOGIC; 
           ce            : in  std_logic;                		 
           go            : in  std_logic;                		 
		   active        : out std_logic;

		   reg4x32_CSRA  : in array4x32; 
		   reg4x32_CSRB  : in array4x32;	
           BRAM_dOut     : in std_logic_vector(255 downto 0);	
           reg32x32_dOut : in std_logic_vector(31 downto 0);
					 
		   wr            : out std_logic;
		   add           : out std_logic_vector(  7 downto 0);					 
		   datToMem	     : out std_logic_vector( 31 downto 0);

		   functBus      : out std_logic_vector(95 downto 0)
           );
end game;

architecture RTL of game is
-- Internal signal declarations
-- <include new states>
type stateType is (idle, writeToCSR0, setupGameParameters, initGameArena, initBall, initPaddle, initLives, initScore, waitState, processPaddle, processBall, writeBallToMem, writePaddleToMem, writeWallToMem, writeScoretoMem, writeLivestoMem, endGame, NW, N, NE, SE, S, SW); -- declare enumerated state type
signal NS, CS                                   : stateType; -- declare FSM state 
								                
signal NSWallVec, CSWallVec                     : std_logic_vector(31 downto 0);
signal NSBallVec, CSBallVec                     : std_logic_vector(31 downto 0);
signal NSPaddleVec, CSPaddleVec                 : std_logic_vector(31 downto 0);
								                
signal NSBallXAdd, CSBallXAdd                   : integer range 0 to 31;
signal NSBallYAdd, CSBallYAdd                   : integer range 0 to 31;
signal NSBallDir, CSBallDir                     : std_logic_vector(2 downto 0);
								                
signal NSScore, CSScore                         : integer range 0 to 31;
signal NSLives, CSLives                         : integer range 0 to 31;

-- Clock frequency = 12.5MHz, count 0 - 12,499,999 to create 1 second delay 
-- 100ms delay => count ~ 0 - 1,250,000
signal NSDlyCountMax, CSDlyCountMax             : integer range 0 to 1250000;
signal NSDlyCount, CSDlyCount                   : integer range 0 to 1250000;

signal NSPaddleNumDlyMax, CSPaddleNumDlyMax     : integer range 0 to 31;
signal NSPaddleNumDlyCount, CSPaddleNumDlyCount : integer range 0 to 31;

signal NSBallNumDlyMax, CSBallNumDlyMax         : integer range 0 to 31;
signal NSBallNumDlyCount, CSBallNumDlyCount     : integer range 0 to 31;

signal zone 									: integer; 

begin

asgnFunctBus2_i: functBus <= (others => '0'); -- not currently used 

-- FSM next state and o/p decode process
-- <include new signals in sensitivity list>
NSAndOPDec_i: process (CS, go, 
					   reg4x32_CSRA, reg4x32_CSRB, reg32x32_dOut, 
					   CSWallVec, CSPaddleVec, CSBallXAdd, CSBallYAdd, CSBallDir, CSScore, CSLives, 
					   CSDlyCountMax, CSPaddleNumDlyMax, CSBallNumDlyMax, CSDlyCount, CSPaddleNumDlyCount, CSBallNumDlyCount)
					  
					
begin
   NS 	 		       <= CS;     -- default signal assignments
   NSWallVec           <= CSWallVec;
   NSBallVec           <= CSBallVec;
   NSPaddleVec         <= CSPaddleVec;
   NSBallXAdd          <= CSBallXAdd;
   NSBallYAdd          <= CSBallYAdd;
   NSBallDir           <= CSBallDir;
   NSScore             <= CSScore;
   NSLives             <= CSLives;
   NSDlyCount          <= CSDlyCount;           
   NSPaddleNumDlyCount <= CSPaddleNumDlyCount;
   NSBallNumDlyCount   <= CSBallNumDlyCount; 
   NSDlyCountMax       <= CSDlyCountMax;
   NSDlyCount          <= CSDlyCount;
   NSPaddleNumDlyMax   <= CSPaddleNumDlyMax;
   NSBallNumDlyMax     <= CSBallNumDlyMax;
   active    	       <= '1';             -- default asserted. Deasserted only in idle state. 
   wr   	           <= '0';
   add	               <= "010" & "00000"; -- reg32x32 base address
   datToMem            <= (others => '0');
   zone                <= 0;

  case CS is 
		when idle => 			     
			active  <= '0';  
            if go = '1' then 
				if    reg4x32_CSRA(0)(10 downto 8) = "000" then -- initialise game values and progress to init game arena states 
					NS 					<= setupGameParameters;							
				elsif reg4x32_CSRA(0)(10 downto 8) = "001" then -- play game
					NS <= waitState;
                        
				end if;
			end if;

		when writeToCSR0 =>                                        -- finish. Return done state and return control to host
			wr       <= '1';
            add      <= "000" & "00000"; 						   -- reg4x32_CSRA address = 0 
		    datToMem <=   reg4x32_CSRA(0)(31 downto  8)            -- bits unchanged 
                        & reg4x32_CSRA(0)( 7 downto  2) & "10";    -- byte 0, bit(1) = 1 => FPGA done, bit(0) = 0 => return control to host. Bits 7:2 unchanged
			NS       <= idle;


		when setupGameParameters =>  
		    NSWallVec           <= reg4x32_CSRA(3);
			NSBallXAdd 	        <= to_integer( unsigned(reg4x32_CSRA(2)(28 downto 24)) );
			NSBallYAdd 	        <= to_integer( unsigned(reg4x32_CSRA(2)(20 downto 16)) );
			NSLives             <= to_integer( unsigned(reg4x32_CSRA(2)(12 downto  8)) );
			NSScore             <= to_integer( unsigned(reg4x32_CSRA(2)( 7 downto  0)) );
			NSBallVec           <= reg4x32_CSRA(1);

			NSPaddleVec         <= reg4x32_CSRB(3);
			NSBallDir           <= reg4x32_CSRB(2)(26 downto  24);
			NSDlyCountMax       <= to_integer( unsigned(reg4x32_CSRB(2)(19 downto  0)) );                 
			NSPaddleNumDlyMax   <= to_integer( unsigned(reg4x32_CSRB(1)(28 downto 24)) );
			NSBallNumDlyMax     <= to_integer( unsigned(reg4x32_CSRB(1)(20 downto 16)) );
			NS                  <= initGameArena;

		when initGameArena => -- follow an initialisation sequence 
            -- write wallVec
			wr   	      <= '1';
			add           <= "010" & "01111";               -- reg32x32 row 15
			datToMem      <= CSWallVec;
           	NS            <= initLives;
        when initLives =>                          
			wr   	      <= '1';
			add	          <= "010" & "00001";               -- reg32x32 row 1
			datToMem      <= X"000000" & "000" & std_logic_vector( to_unsigned(CSLives, 5) );  
           	NS            <= initScore;     
		when initScore =>                          
			wr   	      <= '1';
			add	          <= "010" & "00000";               -- reg32x32 row 0 
			datToMem      <= X"000000" & "000" & std_logic_vector( to_unsigned(CSScore, 5) );  
           	NS            <= initBall;
		when initBall =>                                                                              
			wr   	      <= '1';
			add	          <= "010" & std_logic_vector( to_unsigned(CSBallYAdd,5) );  
			datToMem      <= CSBallVec;
           	NS            <= initPaddle;
		when initPaddle =>
			wr   	      <= '1';
			add	          <= "010" & "00010";               -- reg32x32 row 2 
			datToMem      <= CSPaddleVec;
           	NS            <= writeToCSR0;                   -- finish. Return done state and return control to host


		when waitState =>                                   -- increment count and loop in state until value reaches delay value       
			if CSDlyCount = CSDlyCountMax then
	     	    NSDlyCount <= 0;                            -- clear delay counter and process paddle and/or ball
	     	    	     	         	    
   	   	        NS  <= processPaddle;
            else
	   	        NSDlyCount    <= CSDlyCount + 1;                
			end if;
			
	     	    			
        -- read reg4x32_CSRA(0)(9:8) and move paddle left / right, between boundaries
		when processPaddle =>                               -- read CSRB(0)(9:8)
			if CSPaddleNumDlyCount = CSPaddleNumDlyMax then
	     	   NSPaddleNumDlyCount <= 0;                    -- clear counter
		       add	<= "010" & "00010";                     -- reg32x32 row 2 (paddle row) 
			     if reg4x32_CSRB(0)(9) = '1' then           -- if 1 in (9) -> shift paddle left, if not at bit 31 boundary
			        if reg32x32_dOut(31) = '0' then 
				        wr   	      <= '1';
					    add	          <= "010" & "00010";   -- reg32x32 row 2, paddle row address 
					    datToMem      <= reg32x32_dOut(30 downto 0) & '0'; 
					    NSPaddleVec   <= reg32x32_dOut(30 downto 0) & '0';
				    end if;
			      elsif reg4x32_CSRB(0)(8) = '1' then       -- if 1 in (8) -> shift paddle right 
			        if reg32x32_dOut(0) = '0' then 
					    wr   	      <= '1';
					    add	          <= "010" & "00010";   -- reg32x32 row 2, paddle row address 
					    datToMem      <= '0' & reg32x32_dOut(31 downto 1);
					    NSPaddleVec   <= '0' & reg32x32_dOut(31 downto 1);
				    end if;
				  end if;  
			    		  
           	else
	           NSPaddleNumDlyCount <= CSPaddleNumDlyCount + 1; -- increment counter
           	   NS  <= processBall;
           	end if;	
           		

		-- ========= Only partially completed =========
        when processBall => 	
			if CSBallNumDlyCount = CSBallNumDlyMax then
		   	    NSBallNumDlyCount   <= 0;
		   	   
		   	    if CSBallYAdd /= 15 then                              -- if ball is not currently in the wall
		   	        wr <= '1';
		   	        add(7 downto 5)     <= "010";                                                 -- reg32x32 memory bank select 
			        add(4 downto 0)     <= std_logic_vector( to_unsigned(CSBallYAdd, 5) );        -- current row address 
			        datToMem            <= (others => '0');	                                     -- clear our previous ball state
			    end if;

				if CSBallYAdd >= 4 and CSBallYAdd <= 13 and CSBallXAdd >= 1 and CSBallXAdd <= 30 then  
					zone <= 1;
			     			      
					case CSBallDir(2 downto 0) is                          -- we're in middle state, we move linearaly
						when "000" => NS <= S;
						when "001" => NS <= SE;
						when "010" => NS <= SW;
						when "100" => NS <= N;
						when "101" => NS <= NE;
						when "110" => NS <= NW;
                        when others => null; 
					end case;
					
				elsif CSBallYAdd = 15 then                              -- ball is in the wall
				    zone <= 6;
				    if CSBallXAdd = 31 then                             -- corner cases
				        NS <= SE;
				    elsif CSBallXAdd = 0 then
				        NS <= SW;
				    else
                        case CSBallDir(2 downto 0) is                       
                            when "100" => NS <= S;                          -- if we going north, go south
                            when "101" =>                                   -- going NE
                                if CSWallVec(CSBallXAdd - 1) = '1' then     -- check if wall piece
                                    NSWallVec(CSBallXAdd - 1) <= '0';       -- remove wall piece
                                    NSScore <= CSScore +1;
                                    NS <= SW;                               -- mirror bounce
                                else
                                    NS <= SE;                               -- else we hit very top wall
                                end if;
                            when "110" =>                                   -- going NW
                                if CSWallVec(CSBallXAdd + 1) = '1' then     -- check wall piece
                                    NSWallVec(CSBallXAdd + 1) <= '0';       -- remove wall piece
                                    NSScore <= CSScore +1;
                                    NS <= SE;                               -- mirror bounce
                                else
                                    NS <= SW;                               -- else we hit very top wall
                                end if;
                            when others => null; 
                        end case;
					end if;
					NSWallVec(CSBallXAdd) <= '0';                       -- remove ball in wall for next state
			     
				elsif CSBallXAdd = 31 then                                                       -- left wall
			   
					if CSBallYAdd = 3 then                                                       -- Bottom left corner
						zone <= 7;                                  
						if CSPaddleVec(31) = '1' then                                           -- if paddle we bounce 
							NS <= NE;
 
						else                                                                      -- else lose a life
							NSLives <= CSLives - 1;
						    NS <= writeLivestoMem;
						    NSBallNumDlyCount <= 0;
						end if;
             	      
					elsif CSBallYAdd <= 13 then                                                  -- Middle left wall
						zone <= 5;
						case CSBallDir(2 downto 0) is
							when "010" => NS <= SE;  --In SW Out SE
							when "110" => NS <= NE;  --In NW out SE
                            when others => null; 
						end case;

					else                                                                       -- Top left wall
					    zone <= 8;  
					    if CSWallVec(31) = '1' then                                           -- if wall piece we increment score and remove it
					       	NSScore <= CSScore + 1;
							NSWallVec <= "0" & CSWallVec(30 downto 0);
							NS <= SE;
					    elsif CSBallDir(2) = '0' then                                      -- if going south, remove ball from wall and bounce away from corner
					        NS <= SE;
					        NSWallVec(30) <= '0';
					    elsif CSWallVec(30) = '1' then                                     -- else if wall to the right, we remove it and bounce away from corner
					        NSScore <= CSScore + 1;
							NSWallVec <= "00" & CSWallVec(29 downto 0);
							NS <= SE;
					    else                                                               -- else ball go into wall
					        Ns <= NE;
					        NSWallVec(30) <= '1';
					    end if;                                                                   
					end if;
                   
                   
				elsif CSBallXAdd = 0 then                                                       -- Right wall
			   
					if CSBallYAdd = 3 then                                                       -- Bottom left corner
						zone <= 10;
						if CSPaddleVec(31) = '1' then                                           -- if paddle, bounce
							NS <= NW;
                        
						else                                                                      -- else lose a life
							NSLives <= CSLives - 1;
						    NS <= writeLivestoMem;
						    NSBallNumDlyCount <= 0;
						end if;
             	      
					elsif CSBallYAdd <= 13 then                                                  -- Middle Right wall
						zone <= 4;
						case CSBallDir(2 downto 0) is
							when "001" => NS <= SW;  --In SE Out SW
							when "101" => NS <= NW;  --In NE out NW
                            when others => null; 							
						end case;

					else                                                                         -- Top Right wall
						zone <= 9;
					    if CSWallVec(0) = '1' then                                           -- if wall piece we increment score and remove it
					       	NSScore <= CSScore + 1;
							NSWallVec <= CSWallVec(31 downto 1) & "0";
							NS <= SW;
					    elsif CSBallDir(2) = '0' then                              -- if going south, remove ball from wall and bounce away from corner
					        NS <= SW;
					        NSWallVec(1) <= '0';
					    elsif CSWallVec(1) = '1' then                              -- else if wall to the left, we remove it and bounce away from corner
					        NSScore <= CSScore + 1;
							NSWallVec <= CSWallVec(31 downto 2) & "00";
							NS <= SW;
					    else
					        NS <= NW;                                              -- else ball go into wall
					        NSWallVec(1) <= '1';
					    end if;
					end if;
                   
				elsif CSBallYAdd = 14 then                                                       -- just below top wall
					zone <= 3;
					if CSWallVec(CSBallXAdd) = '1' and CSBallDir(2) /= '0' then                     -- if wall piece where ball is and we're not leaving the wall, we remove it and increment score
						NSScore <= CSScore + 1;
						NSWallVec(CSBallXAdd) <= '0';                                  -- remove wall piece the ball hit
						case CSBallDir(2 downto 0) is
						  when "100" => NS <= S;
						  when "101" => NS <= SE;
						  when "110" => NS <= SW;
						  when others => null;
						end case;
					else
					    case CSBallDir(2 downto 0) is
						  when "100" => 
						      NS <= N; 
						      NSWallVec(CSBallXAdd) <= '1';               -- if N, ball goes N, and we put the ball in wall
						  when "101" =>
						      if CSWallVec(CSBallXAdd - 1) = '1' then     -- if NE, check if wall piece to right
						          NS <= SW;
						          NSScore <= CSScore;
						          NSWallVec(CSBallXAdd - 1) <= '0';
						      else                                        -- put ball in wall if no piece
						          NS <= NE;
						          NSWallVec(CSBallXAdd - 1) <= '1';
						      end if;
						  when "110" =>
						      if CSWallVec(CSBallXAdd + 1) = '1' then
						          NS <= SE;
						          NSScore <= CSScore;
						          NSWallVec(CSBallXAdd + 1) <= '0';
						      else
						          NS <= NW; -- can put zone 11 and 12 here
						          NSWallVec(CSBallXAdd + 1) <= '1';
						      end if;
						      
						  when "000" =>                                       -- if going anyway South, the ball is leaving the wall zone
						      NS <= S;
						  when "001" => 
						      NS <= SE;
						  when "010" => 
						      NS <= SW;
						  when others => null; 
						end case;	
					end if;
					
				else                                                                             -- just above paddle
					zone <= 2;
					if CSBallDir(2) = '1' then                         -- if going north anyway
					    NS <= N;
					
					elsif CSPaddleVec(CSBallXAdd) = '1' then                                    -- if paddle and ball beside each other
						if CSPaddleVec(CSBallXAdd - 1) = '0' then                            -- right side of paddle
							if CSBallDir = "010" then                                       -- Coming in from East
                                NS <= N;
                            else 
                                NS <= NE;
                            end if;
						elsif CSPaddleVec(CSBallXAdd + 1) = '0' then                            -- Left side of paddle
                            if CSBallDir = "001" then                                       -- Coming in from West
                                NS <= N;
                            else 
                                NS <= NW;
                            end if;
						else
							case CSBallDir(2 downto 0) is    -- Ball hits middle of paddle
								when "000" => NS <= N;
								when "001" => NS <= NE;
								when "010" => NS <= NW;
								when others => null; 
							end case;  
						end if;                
                                                        
					else                                                                         -- else we lose a lfe
						NSLives <= CSLives - 1;
						NS <= writeLivestoMem;
						NSBallNumDlyCount <= 0;                  
					end if;
			       
				end if;
			  	           
			  			   
           	else -- CSBallNumDlyCount != CSBallNumDlyMax 
           	    if CSLives /= 0 then
				    NSBallNumDlyCount <= CSBallNumDlyCount + 1; -- increment counter
				    NS  <= waitState;
				else
				    NS <= writeLivestoMem;
				end if;
           	end if;
 
		when NW =>                                                          -- BB State for NW Direction
            NSBallXAdd <= CSBallXAdd + 1;                                         
			NSBallYAdd <= CSBallYAdd + 1;
            NsBallDir <= "110";		
			NS <= writeWallToMem;		
      
		when N =>                                                           -- BB State for N Direction
            NSBallXAdd <= CSBallXAdd;                                         
            NSBallYAdd <= CSBallYAdd + 1;
            NsBallDir <= "100";     		
			NS <= writeWallToMem;		
			 
		when NE =>                                                          -- BB State for NE active 
            NSBallXAdd <= CSBallXAdd - 1;                                         
            NSBallYAdd <= CSBallYAdd + 1;
            NsBallDir <= "101";                                                            
			NS <= writeWallToMem;		
			 			 
		when SE =>                                                          -- BB State for SE Direction
            NSBallXAdd <= CSBallXAdd - 1;                                         
            NSBallYAdd <= CSBallYAdd - 1;
            NsBallDir <= "001";		
			NS <= writeWallToMem;		
			 
		when S =>                                                           -- BB State for S Direction
            NSBallXAdd <= CSBallXAdd; 
            NSBallYAdd <= CSBallYAdd - 1;
            NsBallDir <= "000";	
			NS <= writeWallToMem;	
			 	
		when SW =>                                                          -- BB State for S Direction
            NSBallXAdd <= CSBallXAdd + 1;
            NSBallYAdd <= CSBallYAdd - 1;
            NsBallDir <= "010";		
			NS <= writeWallToMem;	
			
		when writeLivestoMem =>
		    NSBallXAdd <= 16;                                                 -- ball back to middle above paddle
			NSBallYAdd <= 3;
			NSPaddleVec <= X"0007c000";      -- PaddleVector Getting Put Back in the middle
			NSBallVec <= X"00010000";      -- putting ballVec back in middle
			NsBallDir <= "100";
		    wr                       <= '1'; 
			add           <= "010" & "00001";               -- reg32x32 row 1
			datToMem      <= (others => '0');							     -- clear vector  
			datToMem      <= std_logic_vector( to_unsigned(CSLives, 32) );
			if CSLives = 0 then                                                               -- if no more lives, we end game
					NS <= endGame;
		    else
			        NS <= initBall;	
			end if;
			
		when writeWallToMem =>            
	        wr                       <= '1'; 	                 -- write new Wall row
			add           <= "010" & "01111";               -- reg32x32 row 15
			datToMem      <= CSWallVec;
			NS <= writeScoretoMem;	
			
		when writeScoretoMem =>
		    wr                       <= '1'; 
			add           <= "01000000";               -- reg32x32 row 0
	   		datToMem      <= (others => '0');							     -- clear vector  
			datToMem      <= std_logic_vector( to_unsigned(CSScore, 32) );
			NS <= writeBallToMem;	 
			
		when writeBallToMem =>                                                           -- write new ball row
		    if CSBallYAdd /= 15 then
                wr                       <= '1'; 					        			      
                add(7 downto 5)          <= "010";                                          -- reg32x32 memory bank select 
                add(4 downto 0)          <= std_logic_vector( to_unsigned(CSBallYAdd, 5) ); -- row address 
   	            datToMem( to_integer(to_unsigned(CSBallXAdd, 5)) ) <= '1';                  -- ball bit asserted
			end if;
			NS <= waitState;
		

		when endGame =>                          
			-- <perform pattern write to arena to indicate game over>7
			NSLives <= NSLives + 1;
			
			wr                       <= '1'; 					        			      
            add(7 downto 5)          <= "010";                                          -- reg32x32 memory bank select 
            add(4 downto 0)          <= std_logic_vector( to_unsigned(CSLives, 5) ); -- row address
	   		datToMem                 <= (others => '0');                             -- clear the Arena using Lives signal as counter
	   		
	   		if CSLives = 15 then
	   		    NS            <= writeToCSR0;            -- finish. Return done state and return control to host
	   		else 
	   		    NS            <= endGame;                -- keep clearing
	   		end if;
  
		when others => 
			null;
	end case;
end process; 


-- Synchronous process registering current FSM state value, and other registered signals
-- registers include chip enable control
stateReg_i: process (clk, rst)
begin
	if rst = '1' then 		
		CS 	                <= idle;		
		CSWallVec           <= (others => '0');
		CSBallVec           <= (others => '0');
		CSPaddleVec         <= (others => '0');
		CSBallXAdd          <= 0;
		CSBallYAdd          <= 0;
		CSBallDir           <= (others => '0');
		CSScore             <= 0;
		CSLives             <= 0;
		CSDlyCount          <= 0;
		CSPaddleNumDlyCount <= 0;
		CSBallNumDlyCount   <= 0;
		CSDlyCountMax       <= 0;
		CSDlyCount          <= 0;
		CSPaddleNumDlyMax   <= 0;
		CSBallNumDlyMax     <= 0;
	elsif clk'event and clk = '1' then 
		if ce = '1' then
			CS 	                <= NS;		
			CSWallVec           <= NSWallVec;      
			CSBallVec           <= NSBallVec;      
			CSPaddleVec         <= NSPaddleVec;    
			CSBallXAdd          <= NSBallXAdd;     
			CSBallYAdd          <= NSBallYAdd;     
			CSBallDir           <= NSBallDir;      
			CSScore             <= NSScore;        
			CSLives             <= NSLives;        
			CSDlyCount          <= NSDlyCount;           
			CSPaddleNumDlyCount <= NSPaddleNumDlyCount;
			CSBallNumDlyCount   <= NSBallNumDlyCount; 
			CSDlyCountMax       <= NSDlyCountMax;
			CSDlyCount          <= NSDlyCount;
			CSPaddleNumDlyMax   <= NSPaddleNumDlyMax;
			CSBallNumDlyMax     <= NSBallNumDlyMax;
		end if;
	end if;
end process; 

end RTL;