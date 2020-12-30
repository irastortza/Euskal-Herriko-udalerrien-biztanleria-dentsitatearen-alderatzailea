with Ada.Text_IO;
procedure Irakurri_Katea (Katea : out String) is
   -- Aurrebaldintza:
   --    F = <c_1 c_2 ... c_n> (c_i, karakterea)
   -- Postbaldintza:
   --    Katea = c_1 c_2 ... c_n
begin
   Ada.Text_IO.Get (Item => Katea );
end Irakurri_Katea;
