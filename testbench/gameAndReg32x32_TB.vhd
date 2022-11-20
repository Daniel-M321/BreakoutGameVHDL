-- Description: gameAndReg32x32_TB testbench 
-- Engineer: Fearghal Morgan
-- National University of Ireland, Galway / viciLogic 
-- Date: 8/2/2021
-- Change History: Initial version

-- The testbench does not write to CSRA, though controls signals wr, add and datToMem

-- Reference: https://tinyurl.com/vicilogicVHDLTips   	A: VHDL IEEE library source code VHDL code
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use work.arrayPackage.all;

entity gameAndReg32x32_TB is end gameAndReg32x32_TB; -- testbench has no inputs or outputs

architecture Behavioral of gameAndReg32x32_TB is
-- component declaration is in package

-- Declare internal testbench signals, typically the same as the component entity signals
-- initialise signal clk to logic '1' since the default std_logic type signal state is 'U' 
-- and process clkStim uses clk <= not clk  
signal clk            : STD_LOGIC := '1'; 
signal rst            : STD_LOGIC;
signal ce             : std_logic;
signal go             : STD_LOGIC;
signal active         : std_logic;

signal reg4x32_CSRA   : array4x32;
signal reg4x32_CSRB   : array4x32;
signal wr             : std_logic;
signal add            : std_logic_vector(  7 downto 0);		   
signal datToMem	      : STD_LOGIC_VECTOR( 31 downto 0);

signal functBus       : std_logic_vector(95 downto 0);

constant period       : time := 20 ns;    -- 50MHz clk
signal   endOfSim     : boolean := false; -- Default FALSE. Assigned TRUE at end of process stim
signal   testNo       : integer;          -- facilitates test numbers. Aids locating each simulation waveform test 

begin

uut: gameAndReg32x32
port map ( clk 		      => clk, 		 
           rst 		      => rst,
           ce             => ce, 		 
           go             => go,
		   active         => active,

		   reg4x32_CSRA   => reg4x32_CSRA,       
		   reg4x32_CSRB   => reg4x32_CSRB,       
						 
		   wr             => wr,
		   add            => add,    
		   datToMem	      => datToMem,
		   
		   functBus       => functBus	 
           );

-- clk stimulus continuing until all simulation stimulus have been applied (endOfSim TRUE)
clkStim : process (clk)
begin
  if endOfSim = false then
     clk <= not clk after period/2;
  end if;
end process;

stim: process -- no process sensitivity list to enable automatic process execution in the simulator
begin 
  report "%N : Simulation Start."; -- generate messages as the simulation executes 
  -- initialise all input signals   
  
  testNo <= 0; 					  -- include a unique test number to help browsing of the simulation waveform     
							      -- apply rst signal pattern, to deassert 0.2*period after the active clk edge
  go            		<= '0';   -- default assignments
  ce            		<= '1';
  reg4x32_CSRA          <= ( others => (others => '0') );        
  reg4x32_CSRB          <= ( others => (others => '0') );        
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 3*period;

---------------------------------------------------------------

--  -- ffffffff1c0b03010001000000001000
  testNo 				<= 1; 
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"ffffffff";     -- wallVec full
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "11100"; -- "000" & ballXAdd(4:0) --28--     
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "01011"; -- "000" & ballYAdd(4:0) --11--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00011"; -- "000" & lives(4:0)    --3--  
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "00000"; -- "000" & score(4:0)      
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- 001f0000060000020204000000000000
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"001F0000";     -- paddleVec, in the right position for the ball to hit the far left paddle piece  

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "110"; -- ball direction (2:0) --NW--  
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)      
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  

-- -- For test 2 we have set the ball to go into the top left corner below the wall.

  testNo 				<= 2;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array

  wait for 200*period;
  
  
-----------------------------------------------------  
  testNo 				<= 3;       
  
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 30*period;
  
 -- 7fefffff1c0b03020001000000001000
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"7fefffff";     -- wallVec missing far left piece from test 2, and a middle piece from the ball hitting the left side paddle and going straight up
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "11100"; -- "000" & ballXAdd(4:0) --28--     
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "01011"; -- "000" & ballYAdd(4:0) --11--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00011"; -- "000" & lives(4:0)    --3--  
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "00010"; -- "000" & score(4:0)    --2-- from hitting the wall twice  
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- 001F0000060000020204000000000000
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"001F0000";     -- paddleVec, in the right position for the ball to hit the far left paddle piece  

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "110"; -- ball direction (2:0) --NW--  
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)  
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;
  
  -- -- For test 4 we have the ball heading to the same corner with the relevant wall pieces missing
  
  testNo 				<= 4;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array  
  
  wait for 200*period;
  
  -----------------------------------------------------  
  testNo 				<= 5;       
  
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 40*period;
  
 -- 3fefffff1c0b03030001000000001000
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"3fefffff";     -- wallVec missing two far left pieces now and same middle piece
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "11100"; -- "000" & ballXAdd(4:0) --28--
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "01011"; -- "000" & ballYAdd(4:0) --11--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00011"; -- "000" & lives(4:0)    --3--  
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "00011"; -- "000" & score(4:0)    --3--  
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- 001F0000060000020204000000000000
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"001F0000";     -- paddleVec, in the right position for the ball to hit the far left paddle piece

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "110"; -- ball direction (2:0) --NW   
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)  
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;
  
  -- -- For test 6 the ball is hitting once again the same top left corner, but now we have two wall pieces missing from tests 2 & 4.
  -- -- Therefore the ball will hit the left wall, bounce and enter the wall and hit another wall piece in its top corner and mirror bounce back out the wall. The score is also incremented and the hit wall piece is removed.
  -- -- We will hit the left wall again and head towards the left piece of the paddle and when the ball hits the paddle it will go straight up, due to the rebound strategy.
  -- -- Since the wall piece in the balls new path is missing from before, the ball will enter the wall hit the top wall and leave the wall again, without incrementing score since we didnt hit a wall piece.
  
  testNo 				<= 6;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array  
  
  wait for 1000*period;
  
  -----------------------------------------------------  
  testNo 				<= 7;       
  
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 40*period;
  
 -- ffffffff1c0703000001000000001000
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"ffffffff";     -- wallVec full
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "11100"; -- "000" & ballXAdd(4:0) --28--     
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "00111"; -- "000" & ballYAdd(4:0) --7--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00011"; -- "000" & lives(4:0)    --3--      
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "00000"; -- "000" & score(4:0)      
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- 001F0000060000020204000000000000   
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"001F0000";     -- paddleVec  

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "110"; -- ball direction (2:0) --NW--   
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)    
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;
  
  -- -- For test 8 we are showcasing the ball hitting the left wall at an angle and bouncing back towards the top wall. 
  -- -- The wall is full, so when the ball hits the wall at an angle, the hit wall piece is removed, score is incremented, and the ball piece is bounced back at an angle towards the paddle zone.
  
  testNo 				<= 8;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array  
  
  wait for 350*period;
  
  
  -----------------------------------------------------  
  testNo 				<= 9;       
  
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 40*period;
  
 -- ff0000ff170b03100001000000001000
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"ff0000ff";     -- wallVec missing the 16 middle pieces
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "10111"; -- "000" & ballXAdd(4:0) --23--      
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "01011"; -- "000" & ballYAdd(4:0) --11--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00011"; -- "000" & lives(4:0)    --3--  
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "10000"; -- "000" & score(4:0)    --16-- since 16 wall pieces missing  
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- 001F0000050000020204000000000000   
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"001F0000";     -- paddleVec  

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "101"; -- ball direction (2:0) --NE--  
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)    
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;
  
  -- -- For test 10 the ball enters the wallVec at an angle (NE) and leaves in the corresponding angle (SE)
  
  testNo 				<= 10;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array  
  
  wait for 200*period;
  
  
  -----------------------------------------------------  
  testNo 				<= 11;       
  
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 40*period;
  
 -- ffffffff1c0603000001000000001000
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"ffffffff";     -- wallVec full
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "11100"; -- "000" & ballXAdd(4:0) --28--     
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "00110"; -- "000" & ballYAdd(4:0) --6--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00011"; -- "000" & lives(4:0)    --3--  
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "00000"; -- "000" & score(4:0)      
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- f8000000020000020204000000000000   
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"f8000000";     -- paddleVec is against the left wall 

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "010"; -- ball direction (2:0) --SW--  
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)    
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;
  
  -- -- For test 12 the ball is heading SW to the bottom left corner, but the paddle is already there.
  -- -- Therefore the ball will mirror bounce out of the corner back up NE
  
  testNo 				<= 12;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array  
  
  wait for 200*period;
  
    -----------------------------------------------------  
  testNo 				<= 13;       
  
  rst    				<= '1';
  wait for 1.2 * period;
  rst    				<= '0';
  wait for 40*period;
  
 -- ffffffff1c0602000001000000001000
  reg4x32_CSRA                 <= ( others => (others => '0') ); -- clear all CSRA array         
  
  reg4x32_CSRA(3)              <= X"ffffffff";     -- wallVec full
  
  reg4x32_CSRA(2)(31 downto 24)<= "000" & "11100"; -- "000" & ballXAdd(4:0) --28--     
  reg4x32_CSRA(2)(23 downto 16)<= "000" & "00110"; -- "000" & ballYAdd(4:0) --6--     
  reg4x32_CSRA(2)(15 downto  8)<= "000" & "00010"; -- "000" & lives(4:0)    --2--  
  reg4x32_CSRA(2)( 7 downto  0)<= "000" & "00000"; -- "000" & score(4:0)      
  
  reg4x32_CSRA(0)(15 downto 8) <= "00010" & "000"; -- Initialise game. At top DPSProc level, (0) would also be asserted 
  
-- f8000000020000020204000000000000   
  reg4x32_CSRB                 <= ( others => (others => '0') ); -- clear all CSRA array         

  reg4x32_CSRB(3)              <= X"f8000000";     -- paddleVec against left wall

  reg4x32_CSRB(2)(31 downto 24)<= "00000" & "010"; -- ball direction (2:0) --SW--  
  reg4x32_CSRB(2)(19 downto  0)<= X"00002";        -- dlyCount(19:0) 

  reg4x32_CSRB(1)(31 downto 24)<= "000" & "00010"; -- "000" & paddleNumDlyMax(4:0)      
  reg4x32_CSRB(1)(23 downto 16)<= "000" & "00100"; -- "000" & ballNumDlyMax(4:0)    
  
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;
  
  -- -- For test 14 we have the same parameters as test 12, but this time our paddle will continously move right and we have 2 lives left.
  -- -- Therefore the ball will not rebound off the paddle in time and will lose a life and the game will reset the ball and paddle back in the middle and we have one life.
  -- -- The ball will go N towards the wall and bounce back down to the paddle zone. But since our paddle is moving continously right, the ball will miss the padde again.
  -- -- We are out of lives now and the game has ended, the arena is therefore cleared.
  
  testNo 				<= 14;      
  reg4x32_CSRA(0)       <= X"00001101"; -- DSPProc command (15:8) = 0b00010 001, (0) = 1. Play game 
  go     				<= '1'; 
  wait for period;  
  go     				<= '0';   
  wait for 20*period;  
  reg4x32_CSRB       <= ( others => (others => '0') ); -- clear all CSRB array  
  reg4x32_CSRB(0)(9 downto 8) <= "01"; -- assert right control bit        
  wait for 30*period; 
  
  wait for 200*period;

  
  go <= '1';
  
  wait for 10*period;
  
  go <= '0';
  
  wait for 1000*period; -- 20 ns = 1 period up and down takes just les then 10000ns 


  
  endOfSim 				<= true;  -- assert flag. Stops clk signal generation in process clkStim
  report "simulation done";   
  wait;                           -- include to prevent the stim process from repeating execution, since it does not include a sensitivity list
  
end process;

end Behavioral;