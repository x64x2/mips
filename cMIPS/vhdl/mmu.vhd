library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


entity cam is
  generic (DEPTH  :integer := 5); -- bits
  port (clk       :in  std_logic; -- Clock
        rset      :in  std_logic; -- Reset
        cam_enable :in  std_logic; -- Enable search
        cam_addr_in :in  std_logic_vector (DEPTH-1 downto 0); -- Inp row address
        cam_ip_in   :in  std_logic_vector (4*8-1 downto 0); -- IP address
        cam_mac_in  :in  std_logic_vector (6*8-1 downto 0); -- MAC address
        cam_data_wr :in  std_logic; -- Write entry
        cam_hit_out :out std_logic; -- Match has happened
        cam_addr_out:out std_logic_vector (DEPTH-1 downto 0); -- Out row address
        cam_mac_out:out std_logic_vector (6*8-1 downto 0)  -- Output MAC address
        );
end entity;

architecture rtl of cam is
  type arp_record is record
    ipaddress : std_logic_vector(4*8-1 downto 0);
    macaddress: std_logic_vector(6*8-1 downto 0);
  end record;

  type RAM is array (integer range <>) of arp_record;
  signal arpcache : RAM (0 to 2**DEPTH-1);

begin

  process (clk,arpcache,rset,cam_ip_in,cam_mac_in,cam_addr_in)
  begin
    if (rset = '1') then
      for row in 0 to 2**DEPTH-1 loop
        arpcache(row).ipaddress <= x"00000000";
        arpcache(row).macaddress  <= x"000000000000";
      end loop;
      cam_hit_out  <= '0';
      cam_addr_out <= (others=>'X');
      cam_mac_out <= (others=>'X');
    elsif (rising_edge(clk)) then
      if (cam_data_wr = '1') then
        arpcache(conv_integer(cam_addr_in)).ipaddress <= cam_ip_in;
        arpcache(conv_integer(cam_addr_in)).macaddress  <= cam_mac_in;
      elsif(cam_enable = '1') then
        cam_hit_out   <= '0';
        cam_addr_out  <= (others=>'X');
        cam_mac_out   <= (others=>'X');
        cam_loop: for row in 0 to 2**DEPTH-1 loop
          if (arpcache(row).ipaddress = cam_ip_in) then
            cam_hit_out   <= '1';
            cam_addr_out  <= conv_std_logic_vector(row,DEPTH);
            cam_mac_out   <= arpcache(row).macaddress;
            exit cam_loop;
          end if;
        end loop;
      else
        cam_hit_out  <= '0';
        cam_addr_out <= (others=>'X');
        cam_mac_out <= (others=>'X');
      end if;
    end if;
  end process;
end architecture;

