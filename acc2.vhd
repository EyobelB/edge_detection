-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  Eyobel Berhane - s212287@student.dtu.dk
--             :  YOUR NAME HERE - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be build
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   25-11-2021     Final version
--             :
--
-- -----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The entity for task two. Notice the additional signals for the memory.
-- reset is active high.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.types.all;

entity acc is
    port(
        clk    : in  bit_t;             -- The clock.
        reset  : in  bit_t;             -- The reset signal. Active high.
        addr   : out halfword_t;        -- Address bus for data.
        dataR  : in  word_t;            -- The data bus.
        dataW  : out word_t;            -- The data bus.
        en     : out bit_t;             -- Request signal for data.
        we     : out bit_t;             -- Read/Write signal for data.
        start  : in  bit_t;
        finish : out bit_t
    );
end acc;

--------------------------------------------------------------------------------
-- The desription of the accelerator.
--------------------------------------------------------------------------------


architecture rtl of acc is

    -- All internal signals are defined here
    
    --type state_type is (s_init, s_read_request, s_write_request, s_write, s_is_end, s_end);
    type state_type is (s_init, s_read, s_load_window_a, s_load_window_b, s_load_window_c, s_load_window_d, s_load_window_e, s_load_window_f, s_write, s_is_line_end, s_is_end, s_end);
    signal next_state : state_type;
    signal state : state_type := s_init;
    signal addr_int : halfword_t := (others => '0');
    signal addr_next : halfword_t := (others => '0');
    
    shared variable s11, s12, s13, s21, s23, s31, s32, s33, dn : Std_logic_vector(11 downto 0);
    shared variable dx, dy : signed(11 downto 0);
    
    signal reg_a, next_reg_a, reg_b, next_reg_b, reg_c, next_reg_c, reg_d, next_reg_d, reg_e, next_reg_e, reg_f, next_reg_f : word_t;

--signal line1: word_t;

begin    
    data_flip : process(all)
    begin
        en <= '0';
        we <= '0';
        finish <= '0';
        next_reg_a <= reg_a;
        next_reg_b <= reg_b;
        next_reg_c <= reg_c;
        next_reg_d <= reg_d;
        next_reg_e <= reg_e;
        next_reg_f <= reg_f;
        addr <= Std_logic_vector(unsigned(addr_int));
        case(state) is
            when s_init =>
                if (start = '1') then
                    next_state <= s_read;
                end if;
            when s_read =>
                en <= '1';
                next_state <= s_load_window_a;
                addr_next <= Std_logic_vector(unsigned(addr_int) + 88);
            when s_load_window_a =>
                en <= '1';
                next_reg_a <= dataR;
                addr_next <= Std_logic_vector(unsigned(addr_int) + 88);                            
                next_state <= s_load_window_b;
            when s_load_window_b =>
                en <= '1';
                next_reg_b <= dataR;
                addr_next <= Std_logic_vector(unsigned(addr_int) - 175);                            
                next_state <= s_load_window_c;
            when s_load_window_c =>
                en <= '1';
                next_reg_c <= dataR;
                addr_next <= Std_logic_vector(unsigned(addr_int) + 88);                            
                next_state <= s_load_window_d;
            when s_load_window_d =>
                en <= '1';
                next_reg_d <= dataR;
                addr_next <= Std_logic_vector(unsigned(addr_int) + 88);                            
                next_state <= s_load_window_e;
            when s_load_window_e =>
                en <= '1';
                next_reg_e <= dataR;
                addr_next <= Std_logic_vector(unsigned(addr_int) - 177);                            
                next_state <= s_load_window_f;
            when s_load_window_f =>
                en <= '1';
                next_reg_f <= dataR;                        
                next_state <= s_write;    
            when s_write =>
                en <= '1';
                we <= '1';
                
                s11 := "0000" & reg_a(7 downto 0);
                s12 := "0000" & reg_a(15 downto 8);
                s13 := "0000" & reg_a(23 downto 16);
                s21 := "0000" & reg_b(7 downto 0);
                s23 := "0000" & reg_b(23 downto 16);
                s31 := "0000" & reg_c(7 downto 0);
                s32 := "0000" & reg_c(15 downto 8);
                s33 := "0000" & reg_c(23 downto 16);
                dx := abs(signed(s13) - signed(s11) + signed(s23) + signed(s23) - signed(s21) - signed(s21) + signed(s33) - signed(s31));
                dy := abs(signed(s11) - signed(s31) + signed(s12) + signed(s12) - signed(s32) - signed(s32) + signed(s13) - signed(s33));
                dn := Std_logic_vector(dx+dy);
                
                dataW(7 downto 0) <= dn(7 downto 0);
                
                s11 := "0000" & reg_a(15 downto 8);
                s12 := "0000" & reg_a(23 downto 16);
                s13 := "0000" & reg_a(31 downto 24);
                s21 := "0000" & reg_b(15 downto 8);
                s23 := "0000" & reg_b(31 downto 24);
                s31 := "0000" & reg_c(15 downto 8);
                s32 := "0000" & reg_c(23 downto 16);
                s33 := "0000" & reg_c(31 downto 24);
                dx := abs(signed(s13) - signed(s11) + signed(s23) + signed(s23) - signed(s21) - signed(s21) + signed(s33) - signed(s31));
                dy := abs(signed(s11) - signed(s31) + signed(s12) + signed(s12) - signed(s32) - signed(s32) + signed(s13) - signed(s33));
                dn := Std_logic_vector(dx+dy);
                
                dataW(15 downto 8) <= dn(7 downto 0);
                
                s11 := "0000" & reg_a(23 downto 16);
                s12 := "0000" & reg_a(31 downto 24);
                s13 := "0000" & reg_d(7 downto 0);
                s21 := "0000" & reg_b(23 downto 16);
                s23 := "0000" & reg_e(7 downto 0);
                s31 := "0000" & reg_c(23 downto 16);
                s32 := "0000" & reg_c(31 downto 24);
                s33 := "0000" & reg_f(7 downto 0);
                dx := abs(signed(s13) - signed(s11) + signed(s23) + signed(s23) - signed(s21) - signed(s21) + signed(s33) - signed(s31));
                dy := abs(signed(s11) - signed(s31) + signed(s12) + signed(s12) - signed(s32) - signed(s32) + signed(s13) - signed(s33));
                dn := Std_logic_vector(dx+dy);

                dataW(23 downto 16) <= dn(7 downto 0);
                
                s11 := "0000" & reg_a(31 downto 24);
                s12 := "0000" & reg_d(7 downto 0);
                s13 := "0000" & reg_d(15 downto 8);
                s21 := "0000" & reg_b(31 downto 24);
                s23 := "0000" & reg_e(15 downto 8);
                s31 := "0000" & reg_c(31 downto 24);
                s32 := "0000" & reg_f(7 downto 0);
                s33 := "0000" & reg_f(15 downto 8);
                dx := abs(signed(s13) - signed(s11) + signed(s23) + signed(s23) - signed(s21) - signed(s21) + signed(s33) - signed(s31));
                dy := abs(signed(s11) - signed(s31) + signed(s12) + signed(s12) - signed(s32) - signed(s32) + signed(s13) - signed(s33));
                dn := Std_logic_vector(dx+dy);

                dataW(31 downto 24) <= dn(7 downto 0);
                
                addr <= Std_logic_vector(unsigned(addr_int) + 25344);                             
                next_state <= s_is_line_end;
            when s_is_line_end =>
                if((unsigned(addr_int) + 2) mod 88 = 0) then
                    addr_next <= Std_logic_vector(unsigned(addr_int) + 2);
                    next_state <= s_is_end;
                else
                    addr_next <= Std_logic_vector(unsigned(addr_int) + 1);
                    next_state <= s_read;
                end if;
            when s_is_end =>
                if (to_integer(unsigned(addr_int)) = 25168) then
                    next_state <= s_end;
                else
                    next_state <= s_read;
                end if;
            when s_end =>
                finish <= '1';
                next_state <= s_init;
        end case;
    end process data_flip;
   
    -- Reset signal
   
    sequential : process(clk)
    begin
        if rising_edge(clk) then
            state <= next_state;
            addr_int <= addr_next;
            reg_a <= next_reg_a;
            reg_b <= next_reg_b;
            reg_c <= next_reg_c;
            reg_d <= next_reg_d;
            reg_e <= next_reg_e;
            reg_f <= next_reg_f;
            if (reset = '1') then
                state <= s_init;
            end if;

        end if;
       
    end process sequential;


end rtl;