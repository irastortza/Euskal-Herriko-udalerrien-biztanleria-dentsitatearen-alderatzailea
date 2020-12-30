WITH Idatzi_Osoa,Irakurri_Osoa,Idatzi_Katea,Irakurri_Katea,Idatzi_Erreala,Irakurri_Erreala,Idatzi_Karakterea, Ada.Text_IO,Ada.Integer_Text_IO,Ada.Float_text_IO,irakurri_karakterea;
PROCEDURE Udal_Alderatzailea IS
   --UNITATE BAKOITZA GORDEKO DUEN DATU EGITURAREN ERAZAGUPENA
   Max: CONSTANT Integer := 40;
   Max2: CONSTANT Integer:= 20;

   SUBTYPE Tarte IS Natural RANGE 1 .. Max;
   subtype Tarte2 is natural Range 1.. Max2;
   TYPE Udala IS RECORD
      Izena: String (Tarte);
      Herrialdea: String (Tarte2);
      Biztanleria: Integer;
      Azalera: Float;
      Dentsitatea: Float;
   end record;
   --DATU EGITURA OROKORRAREN ERAZAGUPENA
   TYPE Nodo;
   TYPE Udalsorta IS ACCESS Nodo;
   TYPE Nodo IS RECORD
      Info: Udala;
      Hurrengoa: Udalsorta;
   END RECORD;
   --ALDERATZAILEAREN DATU EGITURA OSAGARRIEN ERAZAGUPENA
   Max3: CONSTANT Integer:= 7;
   SUBTYPE Herrialdeak IS Natural RANGE 1.. Max3;
   TYPE D_Antzekoenak IS ARRAY (Herrialdeak) OF Float;
   TYPE Antzekoenak IS ARRAY (Herrialdeak) OF String (Tarte);
   TYPE Boolear_Herri IS ARRAY (Herrialdeak) OF Boolean;
   --DATU-EGITURA GEHIGARRIAK: EGIAZTATZAILEAREN DATU-EGITURAK
   Max4: CONSTANT Integer := 700;
   SUBTYPE Tarte3 IS Natural RANGE 1..Max4;
   type Egia_Erreg IS RECORD
      Izena: String (Tarte);
      Dentsitatea: Float;
      Herrialdea: String (Tarte2);
   END RECORD;
   TYPE Egia_Bek IS ARRAY (Tarte3) OF Egia_Erreg;
   --------------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------------
   --AZPIPROGRAMEN ERAZAGUPENA
   --BEKTORE SUNTSITZAILEA
   --Aurre: Karaktere-kate bat, edozein luzerakoa eta haren luzera, zenbaki osoa.
   --Post: Luzera bereko karaktere-katea baina haren osagai guztiak hutsak dira.
   procedure Suntsitu (Bek: in out String; Luze: in Integer) is
   BEGIN
      FOR I IN 1..Luze-1 LOOP
         Bek(I):=' ';
      END LOOP;
   END Suntsitu;
   --HUTS SORTZAILEA
   --Aurre: Ez du ezer behar.
   --Post: Udala motako erregistro bat, bere osagai guztiak hutsak edo nuluak direlarik.
   FUNCTION Huts RETURN Udala IS
      Er: Udala;
   BEGIN
      FOR I IN 1..Max LOOP
         Er.Izena(I):=' ';
      END LOOP;
      FOR I IN 1..Max2 LOOP
         Er.Herrialdea(I):=' ';
      end loop;
      Er.Biztanleria:= (0);
      Er.Azalera:=0.0;
      Er.Dentsitatea:=0.0;
      RETURN Er;
   end Huts;

   --HARTZAILEAREN LAGUNTZAILEA
   --Aurre: Karaktere-kate bat eta zenbaki oso bat, haren luzera adierazten duena.
   --Post: Karaktere-kate bat, aurrekoaren berdina baina zabor-osagaiak zirenak (idatzi gabekoak) huts bihurtuta.
   FUNCTION H_Lagun (A: String; B: Integer) return String IS
      Ona: String (1..B);
      I: Integer;
   BEGIN
      I:=A'Length+1;
      Ona:=A;
      WHILE Ona'Length < B LOOP
         Ona(I):=' ';
         I:=I+1;
      END LOOP;
      return Ona;
   END H_Lagun;

   -- INPRIMATZAILEA
   --Aurre: Udalsorta motako lista dinamikoa.
   --Post: Lista dinamikoko osagai guztiak (erregistroak dira, haren osagaiak ere) pantailan inprimatzen dira.
   PROCEDURE Inprimatzailea (L: IN Udalsorta) IS
      Lag: udalsorta;
   BEGIN
      Lag:=L;
      WHILE Lag /= NULL LOOP
         Ada.Text_IO.Put ("Izena: "&Lag.Info.Izena);
         Ada.Text_IO.Put ("Herrialdea: "&Lag.Info.Herrialdea);
         Ada.Text_IO.Put ("Biztanleria: "&Lag.Info.Biztanleria'Image&" biztanle");
         Ada.Text_IO.Put ("Azalera: "&Lag.Info.Azalera'Image&" km^2");
         Ada.Text_IO.Put ("Dentsitatea: "&Lag.Info.Dentsitatea'Image&" biz/km^2");
         Idatzi_katea ("                                    ");
         Lag:=Lag.Hurrengoa;
      END LOOP;
   END Inprimatzailea;

   --BIKI INPRIMATZAILEA
   --Aurre: Antzekoenak motako bektorea.
   --Post: Antzekoenak motako bektorea pantailan inprimatzen da.
   PROCEDURE Biki_Inp (Bek: IN Antzekoenak) IS
   BEGIN
      Ada.Text_IO.Put("ARABA: "&Bek(1));
      Ada.Text_IO.Put("BIZKAIA: "&Bek(2));
      Ada.Text_IO.Put("GIPUZKOA: "&Bek(3));
      Ada.Text_IO.Put("LAPURDI: "&Bek(4));
      Ada.Text_IO.Put("NAFARROA: "&Bek(5));
      Ada.Text_IO.Put("NAFARROA BEHEREA: "&Bek(6));
      Ada.Text_IO.Put("ZUBEROA: "&Bek(7));
   END Biki_Inp;

   --BEKTORE INPRIMATZAILEA
   --Aurre: D_Antzekoenak motako bektorea (errealak).
   --Post: D_Antzekoenak motako bektorea pantailan inprimatzen da.
   PROCEDURE Bek_Inp (Bek: IN D_Antzekoenak) IS
   BEGIN
      Idatzi_Erreala(Bek(1));
      Ada.Text_IO.Put("&&&&");
      Idatzi_Erreala(Bek(2));
      Ada.Text_IO.Put("&&&&");
      Idatzi_Erreala(Bek(3));
      Ada.Text_IO.Put("&&&&");
      Idatzi_Erreala(Bek(4));
      Ada.Text_IO.Put("&&&&");
      Idatzi_Erreala(Bek(5));
      Ada.Text_IO.Put("&&&&");
      Idatzi_Erreala(Bek(6));
      Ada.Text_IO.Put("&&&&");
      Idatzi_erreala(Bek(7));
      Ada.Text_IO.New_Line;
   END Bek_Inp;

   --DENTSITATE KALKULAGAILUA
   --Aurre: Udalsorta motako lista dinamikoa.
   --Post: Udalsorta motako lista dinamiko bera, osagai bakoitzaren dentsitatea azpiosagaia kalkulu egokiarekin beteta.
   PROCEDURE D_Kalk (L: IN Udalsorta) IS
      Lag: Udalsorta;
   BEGIN
      Lag:=L;
      WHILE Lag /= NULL LOOP
         Lag.Info.Dentsitatea:=Float(Lag.Info.Biztanleria)/Lag.Info.Azalera;
         Lag:=Lag.Hurrengoa;
      END LOOP;
   END D_Kalk;

   --KATE ALDERATZAILEA
   --Aurre: A eta B karaktere-kateak.
   --Post: Balio boolearra, zeina TRUE izango den bi karaktere-kateen lehen 10 osagaiak eta 13.a berdinak badira
   --eta FALSE hala ez bada.
   FUNCTION Kate_Aldera (A,B: String) RETURN Boolean IS
   BEGIN
      FOR I IN 1..10 LOOP
         IF A(I) /= B(I) THEN
            RETURN False;
         END IF;
      END LOOP;
      IF A(13) /= B(13) THEN
         RETURN False;
      end if;
      RETURN True;
   END Kate_Aldera;

   --BILATZAILEA
   --Aurre: L udalsorta motako lista dinamikoa eta Zein karaktere-katea, zeina herri-izen bat den gehienetan.
   --Post: Zenbaki erreal bat, Zein karaktere-kateak adierazten duen herriaren dentsitatearen berdina.
   FUNCTION Bilatzailea (L: Udalsorta; Zein: String) RETURN Float IS
      Lag: Udalsorta;
   BEGIN
      Lag:=L;
      WHILE Lag /= NULL LOOP
         IF Kate_aldera(Lag.Info.Izena,Zein) = True THEN
            RETURN Lag.Info.Dentsitatea;
         END IF;
         Lag:=Lag.Hurrengoa;
      END LOOP;
      RETURN 0.0;
   END Bilatzailea;

   --ALDERANTZIZKO BILATZAILEA
   --Aurre: L udalsorta motako litsa dinamikoa, zein dentsitate datua (zenbaki erreala), non karakterea (herrialde baten lehena)
   --Post: Zein dentsitatea duen eta non karaktereaz hasten den herrialdean kokatzen den herriaren izena adierazten duen karaktere-katea.
   FUNCTION A_Bilatzailea (L: Udalsorta; Zein: Float; non: Character) RETURN String IS
      Lag: Udalsorta;
      kutxa: String (Tarte2);
   BEGIN
      Lag:=L;
      WHILE Lag /= NULL LOOP
         Kutxa:=Lag.Info.Herrialdea;
         IF Kutxa(10) = 'B' THEN
            Kutxa(1):='H';
         END IF;
         IF Lag.Info.Dentsitatea = Zein and non = kutxa(1) THEN
            RETURN Lag.Info.Izena;
         END IF;
         Lag:=Lag.Hurrengoa;
      END LOOP;
   END A_Bilatzailea;

   --EMAN HERRIALDEA
   --Aurre: Zein zenbaki osoa.
   --Post: Zenbaki horri dagokion herrialdearen lehen karakterea.
   FUNCTION Eman_Herrialdea (Zein: Integer) RETURN Character IS
   BEGIN
      IF Zein = 5 THEN
         RETURN ('N');
      ELSIF Zein = 1 THEN
         RETURN ('A');
      ELSIF Zein = 2 THEN
         RETURN ('B');
      ELSIF Zein = 3 THEN
         RETURN ('G');
      ELSIF Zein = 4 THEN
         RETURN ('L');
      ELSIF Zein = 7 THEN
         RETURN ('Z');
      ELSE
         RETURN ('H');
      END IF;
   END Eman_Herrialdea;

   --TXIKIENAREN BILATZAILEA
   --Aurre: Her Udala motako erregistroa, alderatokia D_Antzekoenak motako bektorea
   --Post: Zenbaki erreala, D_Antzekoenak motako bektorean dagoena eta Her erregistroari esker zehaztutako herrialdeko herri zehatz baten dentsitate-diferentzia adierazten duena.
   PROCEDURE Eman_Txikiena (Her: IN Udala; Alderatokia: IN D_Antzekoenak; Txiki: OUT Float) IS
   BEGIN
      IF Her.Herrialdea(1) = 'B' THEN
         Txiki:=Alderatokia(2);
      ELSIF Her.Herrialdea(1) = 'G' THEN
         txiki:=Alderatokia(3);
      ELSIF Her.Herrialdea(1) = 'N' and Her.Herrialdea(10) /= 'B' THEN
         Txiki:=Alderatokia(5);
      ELSIF Her.Herrialdea(1) = 'L' THEN
         txiki:=Alderatokia(4);
      ELSIF Her.Herrialdea(1) = 'A' THEN
         Txiki:=Alderatokia(1);
      ELSIF Her.Herrialdea(1) = 'Z' THEN
         Txiki:=Alderatokia(7);
      ELSE
         Txiki:=Alderatokia(6);
      END IF;
   END Eman_Txikiena;

   --BILTEGIRATZAILEA
   --Aurre: Her Udala motako erregistroa, dents zenbaki erreala, alderatokia D_antzekoenak motako erregistroa, d_tokia D_antzekoenak motako erregistroa.
   --Post: Dentsitate-diferentziak eta dentsitate datu zehatzak gordetzen dituzten bi D_antzekoenak motako bektoreak, eguneratuta datu berriekin.
   PROCEDURE Gorde_TBerria (Her: in Udala; dents: in Float; alderatokia: in out D_antzekoenak; d_tokia: in out D_antzekoenak; txiki: in Float) IS
   BEGIN
      IF Her.Herrialdea(1) = 'B' THEN
         Alderatokia(2):=Txiki;
         D_tokia(2):=dents;
      ELSIF Her.Herrialdea(1) = 'G' THEN
         Alderatokia(3):=Txiki;
          D_tokia(3):=dents;
      ELSIF Her.Herrialdea(1) = 'N' and Her.Herrialdea(10) /= 'B' THEN
         Alderatokia(5):=Txiki;
          D_tokia(5):=dents;
      ELSIF Her.Herrialdea(1) = 'L' THEN
         Alderatokia(4):=Txiki;
         D_Tokia(4):=Dents;
      ELSIF Her.Herrialdea(1) = 'A' THEN
         Alderatokia(1):=Txiki;
         D_Tokia(1):=Dents;
      ELSIF Her.Herrialdea(1) = 'Z' THEN
         Alderatokia(7):=Txiki;
         D_Tokia(7):=Dents;
      ELSE
         Alderatokia(6):=Txiki;
          D_tokia(6):=dents;
      END IF;
   END Gorde_TBerria;

   --EMAITZA HASIERATZAILEA
   --Aurre: L udalsorta motako lista dinamikoa, dents aztergai den herriaren dentsitatea adierazten duen zenbaki erreala.
   --Post: Em eta Em2 D_antzekoenak motako bektoreak, behin behinean herri antzekoenen dentsitate-diferentziak eta dentsitateak gordeko dituztenak, hasieratuta datubaseko balio hurbilenekin.
   PROCEDURE Emaitza_Hasi (L: IN Udalsorta; Dents: in Float; Em, Em2: OUT D_Antzekoenak) IS
      Lag: Udalsorta;
      Bol: Boolear_Herri;
      kalk: Float;
   BEGIN
      Lag:=L;
      Bol:=(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
      WHILE Bol /= (TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE) LOOP
         IF Bol(1) = FALSE AND THEN Lag.Info.Herrialdea(1) = 'A' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
               Kalk:=Kalk*(-1.0);
            END IF;
            Em(1):=kalk;
            Em2(1):=Lag.Info.Dentsitatea;
            Bol(1):=TRUE;
         END IF;
         IF Bol(2) = FALSE AND THEN Lag.Info.Herrialdea(1) = 'B' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
              Kalk:=Kalk*(-1.0);
            end if;
            Em(2):=kalk;
            Em2(2):=Lag.Info.Dentsitatea;
            Bol(2):=TRUE;
         END IF;
         IF Bol(3) = FALSE AND THEN Lag.Info.Herrialdea(1) = 'G' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
               Kalk:=Kalk*(-1.0);
            end if;
            Em(3):=kalk;
            Em2(3):=Lag.Info.Dentsitatea;
            Bol(3):=TRUE;
         END IF;
         IF Bol(4) = FALSE AND THEN Lag.Info.Herrialdea(1) = 'L' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
               Kalk:=Kalk*(-1.0);
            end if;
            Em(4):=kalk;
            Em2(4):=Lag.Info.Dentsitatea;
            Bol(4):=TRUE;
         END IF;
         IF Bol(5) = FALSE AND Lag.Info.Herrialdea(1) = 'N' AND Lag.Info.Herrialdea(10) /= 'B' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
               Kalk:=Kalk*(-1.0);
            end if;
            Em(5):=kalk;
            Em2(5):=Lag.Info.Dentsitatea;
            Bol(5):=TRUE;
         END IF;
         IF Bol(7) = FALSE AND THEN Lag.Info.Herrialdea(1) = 'Z' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
               Kalk:=Kalk*(-1.0);
            END IF;
            Em(7):=kalk;
            Em2(7):=Lag.Info.Dentsitatea;
            Bol(7):=TRUE;
         END IF;
         IF Bol(6) = False AND Lag.Info.Herrialdea(1) = 'N' AND Lag.Info.Herrialdea(10) = 'B' THEN
            Kalk:=Lag.Info.Dentsitatea-Dents;
            IF Kalk < 0.0 THEN
               Kalk:=Kalk*(-1.0);
            END IF;
            Em(6):=kalk;
            Em2(6):=Lag.Info.Dentsitatea;
            Bol(6):=TRUE;
         END IF;
         Lag:=Lag.Hurrengoa;
      END LOOP;
   END Emaitza_Hasi;

   --ALDERATZAILEA
   --Aurre: L udalsorta motako lista dinamikoa, Herria aztergai den herriaren izena duen karaktere-katea.
   --Post: Antzekoenak motako bektorea, aztergai den herriaren herri antzekoenen behin betirako datuak dituena.
   PROCEDURE Alderatzailea (L: IN Udalsorta; Herria: IN String; Bikiak: OUT Antzekoenak) IS
      Lag: Udalsorta;
      H_Dents: Float;
      Aldera, Txikiena: Float;
      Emaitzak: D_Antzekoenak;
      D_Emaitzak: D_Antzekoenak;
   BEGIN
      Lag:=L;
      H_Dents:=Bilatzailea (L,Herria);
      Emaitza_Hasi (Lag,H_Dents,Emaitzak,D_Emaitzak);
      WHILE Lag /= NULL LOOP
         Aldera:=Lag.Info.Dentsitatea-H_Dents;
         Eman_Txikiena(Lag.Info,Emaitzak,Txikiena);
         IF Aldera < 0.0 THEN
            Aldera:=Aldera*(-1.0);
         END IF;
         IF Aldera < Txikiena THEN
            Gorde_TBerria (Lag.Info,Lag.Info.Dentsitatea,Emaitzak,D_Emaitzak,Aldera);
         END IF;
         Lag:=Lag.Hurrengoa;
      END LOOP;
      Lag:=L;
      FOR I IN Herrialdeak LOOP
         Bikiak(I) := A_Bilatzailea (Lag,D_Emaitzak(I),Eman_Herrialdea(I));
      END LOOP;
   END Alderatzailea;

   --ESKATZAILEAREN LAGUNTZAILEA
   --Aurre: A karaktere-katea, erabiltzaileak sartutako edozein luzerako karakterez osatua (gehienetan). Nora karaktere-katean balizkoak diren karaktere kopurua (zenbaki osoa)
   --eta M karaktere-kate horren luzera maximoa adierazten duen zenbaki osoa.
   --Post: A karaktere-katearen berdina den karaktere-katea, baina zabor-osagaiak (erabiltzaileak idatzi ez dituenak) hustuneekin ordezkatuta.
   FUNCTION Eska_Lagun (A: String; Nora, M: Integer) RETURN String IS
      Ona: String (1..M);
   BEGIN
      FOR I IN 1..Nora LOOP
         Ona(I):=A(I);
      end loop;
      FOR I IN Nora+1..M LOOP
         Ona(I):=' ';
      END LOOP;
      RETURN Ona;
   END Eska_Lagun;

   --ESKATZAILEA
   --Aurre: Ez du ezer behar.
   --Post: Erabiltzaileak sartutako herriaren izena duen karaktere-katea zabor-osagaiak (berak sartu gabekoak) ezabatuta eta luzera maximoa ezarrita automatikoki.
   FUNCTION Eskatzailea RETURN String IS
      Kar: Character;
      Zein: String (Tarte);
      Kont: Integer;
   BEGIN
      Kont:=1;
      Ada.Text_IO.Put("Zein herriren 'bikiak' erakustea nahi duzu? (Amaieran puntua ipini) ");
      Irakurri_karakterea(Kar);
      WHILE Kar /= '.' LOOP
         Zein(Kont):=Kar;
         Kont:=Kont+1;
         Irakurri_karakterea(Kar);
      END LOOP;
      kont:=kont-1;
      Zein:=Eska_Lagun(Zein,Kont,Max);
      RETURN Zein;
   END Eskatzailea;

   --AZKEN INPRIMATZAILEA
   --Aurre: Antzekoenak motako bektorea eta bilatu den herriaren izena duen karaktere-kate Jat.
   --Post: Antzekoenak motako bektoreko osagaiak eta Jatorrizko herriaren izena inprimatuta daude pantailan.
   PROCEDURE Azken_Inp (Bek: IN Antzekoenak; Jat: IN String) IS
      Kont: Integer;
   BEGIN
      Kont:=1;
      WHILE Jat(Kont) /= ' ' LOOP
         Idatzi_Karakterea (Jat(Kont));
         kont:=kont+1;
      end loop;
      Ada.Text_IO.Put ("ren herri 'bikiak': ");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("* Araban: "&Bek(1));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("* Bizkaian: "&Bek(2));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("* Gipuzkoan: "&Bek(3));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("* Lapurdin: "&Bek(4));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("* Nafarroan: "&Bek(5));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("* Nafarroa Beherean: "&Bek(6));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("* Zuberoan: "&Bek(7));
   END Azken_Inp;

   --GIDA
   --Aurre: Erabiltzaileak sartutako bide-izena (azpiprogramak berak eskatzen du).
   --Post: Bide-izena bera, baina behar den maximora arte hutsunez beteta (erabiltzaileari lana kenduz).
   FUNCTION Gida RETURN String IS
      I: Integer;
      Kar: Character;
      Gida: String (1..100);
   BEGIN
      Idatzi_katea ("Sartu fitxategiaren bide-izena: ");
      I:=1;
      Irakurri_Karakterea (Kar);
      Irakurri_Karakterea(Kar);
      WHILE Kar /= '"' LOOP
         Gida(I):=Kar;
         Irakurri_Karakterea (Kar);
         I:=I+1;
      END LOOP;
      Gida:=Eska_Lagun (Gida,I,100);
      RETURN Gida;
   END Gida;

   --HARTZAILEA
   --Aurre: Non datubasearen bide-izena duen karaktere-katea.
   --Post: Udalsorta motako lista dinamikoa, datubaseko datu guztiak kargatuta dituena eta herri barneko datu eta herri arteko datu bereizketak egina dituena.
   PROCEDURE Hartzailea (L: OUT Udalsorta; Non: in String) IS
      Irakur: Character;
      Irakur2: Integer;
      Irakur3: Float;
      Biz: String (Tarte);
      Biz2: String (Tarte2);
      J: Integer;
      Fitxa: Ada.Text_IO.File_Type;
      La: Udala;
   BEGIN
      Ada.Text_IO.Open(File=>Fitxa,Mode=>Ada.Text_IO.In_File,Name =>Non);
      Ada.Text_IO.Skip_Line(Fitxa);
      WHILE Ada.Text_IO.End_Of_File (Fitxa) = FALSE LOOP
         Ada.Integer_Text_IO.Get(Fitxa,Irakur2);
         La.Biztanleria:=Irakur2;
         Ada.Text_IO.Get(Fitxa,Irakur);
         Ada.Float_Text_IO.Get(Fitxa,Irakur3);
         La.Azalera:=Irakur3;
         Ada.Text_IO.Get(Fitxa,Irakur);
         Ada.Text_IO.Get(Fitxa,Irakur);
         J:=1;
         Suntsitu(Biz,max);
         WHILE Irakur /= ',' LOOP
            Biz(J):=Irakur;
            J:=J+1;
            Ada.Text_IO.Get(Fitxa,Irakur);
         END LOOP;
         Biz:=H_Lagun(Biz,Max);
         La.Izena:=Biz;
         Ada.Text_IO.Get(Fitxa,Irakur);
         J:=1;
         Suntsitu(Biz,max2);
         WHILE not Ada.Text_IO.End_Of_Line(Fitxa) LOOP
            Biz2(J):=Irakur;
            J:=J+1;
            Ada.Text_IO.Get(Fitxa,Irakur);
         END LOOP;
         Biz2(J):=Irakur;
         Biz2:=H_Lagun(Biz2,Max2);
         La.Herrialdea:=Biz2;
         Suntsitu(Biz2,Max2);
         L:=NEW Nodo'(La,L);
      END LOOP;
      Ada.Text_IO.Close(Fitxa);
   END Hartzailea;

   --HERRI EGIAZTATZAILEA
   --Aurre: Udalsorta motako lista dinamikoa eta herri baten ustezko izena duen karaktere-katea.
   --Post: Balio boolearra, TRUE dena karaktere-katea datubaseko datu baten izenaren berdina bada eta FALSE dena hala ez bada.
   FUNCTION Egia_Herria (L: Udalsorta; Herria: String) RETURN Boolean IS
      Em: Float;
   BEGIN
      Em:=Bilatzailea (L,Herria);
      IF Em = 0.0 THEN
         RETURN False;
      ELSE
         RETURN True;
      END IF;
   END Egia_Herria;

   --SORTU FITXA
   --Aurre: Udalsorta motako lista dinamikoa eta bide-izena duen Non karaktere-katea.
   --Post: Fitxategi batean inprimatu dira datubaseko datu guztiak.
   PROCEDURE Sortu_Fitxa (L: IN Udalsorta; Non: in String) IS
      Emaitzak: Antzekoenak;
      Lag: Udalsorta;
      Fitxa: Ada.Text_IO.File_Type;
      Zein:String (Tarte);
      Tokia: String (1..100);
   BEGIN
      Lag:=L;
      Hartzailea (Lag, non);
      D_Kalk (Lag);
      Lag:=L;
      Tokia:=Gida;
      Ada.Text_IO.Create(Fitxa,Ada.Text_IO.Out_File,Tokia);
      Ada.Text_IO.Put(Fitxa,"HERRIA"&"&");
      Ada.Text_IO.Put(Fitxa,"Araba"&"&");
      Ada.Text_IO.Put(Fitxa,"Bizkaia"&"&");
      Ada.Text_IO.Put(Fitxa,"Gipuzkoa"&"&");
      Ada.Text_IO.Put(Fitxa,"Lapurdi"&"&");
      Ada.Text_IO.Put(Fitxa,"Nafarroa"&"&");
      Ada.Text_IO.Put(Fitxa,"Nafarroa Beherea"&"&");
      Ada.Text_IO.Put(Fitxa,"Zuberoa"&"//");
      Ada.Text_Io.New_Line(Fitxa);
      WHILE Lag /= NULL LOOP
         Zein:=(Lag.Info.Izena);
         Alderatzailea (L,Zein,Emaitzak);
         Ada.Text_IO.Put(Fitxa,Zein&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(1)&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(2)&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(3)&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(4)&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(5)&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(6)&"&");
         Ada.Text_IO.Put(Fitxa,Emaitzak(7)&"//");
         Ada.Text_IO.New_Line (Fitxa);
         Lag:=Lag.Hurrengoa;
      END LOOP;
      Ada.Text_IO.Close(Fitxa);
   END Sortu_Fitxa;

   ----------------------------------------------------------------------------------------------------
   ------------------------------EGIAZTAPENA: AZPIPROGRAMA GEHIGARRIAK---------------------------------
   --Aurre: Egia_Bek motako bektorea (datubaseko datu batzuk dituena) eta udalkop zenbaki osoa, udalen kopurua adierazten duena.
   --Post: Egia_Bek motako bektorea, zeinaren osagaiak ordenatuta dauden dentsitatearen arabera txikienetik handienera.
   PROCEDURE Ordenaketa (Bek: IN Out Egia_Bek; udalkop: integer) IS
      Hkop, HandPos: Integer;
      Handiena: Float;
      Kont: Integer;
      BhB: Egia_Erreg;
   BEGIN
      Hkop:=0;
      FOR I IN Bek'First..Udalkop-1 LOOP
         Handiena:=0.0;
         Kont:=1;
         FOR I IN 1..Udalkop-Hkop LOOP
            IF Bek(Kont).Dentsitatea > Handiena THEN
               Handiena:=Bek(Kont).Dentsitatea;
               HandPos:=kont;
             END IF;
             kont:=kont+1;
         END LOOP;
         BhB:=Bek(udalkop-HKop);
         Bek(udalkop-HKop):=Bek(HandPos);
         Bek(HandPos):=BhB;
         HKop:=Hkop+1;
      END LOOP;
   END Ordenaketa;

   --AZPIPROGRMA GEHIGARRIA: EGIAZTATZAILE OROKORRA
   --Aurre: Udalsorta motako lista dinamikoa (datubasea).
   --Post: Datubaseko elementu guztiak inprimatu dira pantailan, dentsitatearen arabera txikienetik handienera.
   PROCEDURE Egiaz_Orokor (L: IN Udalsorta) IS
      Lag: Udalsorta;
      I, J: Integer;
      Bek: Egia_Bek;
   begin
      Lag:=L;
      I:=1;
      WHILE Lag /= NULL LOOP
         Bek(I).Dentsitatea:=Lag.Info.Dentsitatea;
         Bek(I).Izena:=Lag.Info.Izena;
         Bek(I).Herrialdea:=Lag.Info.Herrialdea;
         I:=I+1;
         Lag:=Lag.Hurrengoa;
      END LOOP;
      Ordenaketa(Bek,I-1);
      J:=I-1;
      FOR I IN Bek'First..(J) LOOP
         Ada.Text_IO.Put(Bek(I).Izena&Bek(I).Dentsitatea'Image&" "&Bek(I).Herrialdea);
         Ada.Text_IO.New_Line;
      END LOOP;
   END Egiaz_Orokor;
   -------------------------------------------------------------------------------------------------------------------------------
   -------------------------------------------------------------------------------------------------------------------------------
   --------------------------------ALDAGAIEN ERAZAGUPENA
   Udalak: Udalsorta;
   Zein: String (Tarte);
   Emaitza: Antzekoenak;
   Non: String (1..100);
BEGIN
   Non:=Gida;
   Hartzailea (Udalak,non);
   D_Kalk (Udalak);
   Zein:=Eskatzailea;
   IF Egia_Herria (udalak,Zein) = True THEN
      Alderatzailea(Udalak,Zein,Emaitza);
      Azken_Inp (Emaitza,Zein);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("-----------------FITXA SORTZEA-----------------");
      Ada.Text_IO.New_Line;
      Sortu_Fitxa(Udalak, non);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("** Datu guztiak dituen fitxategi bat sortu da. ");
   ELSE
      Ada.Text_IO.Put("ERROREA: Sartutako herria ez dago datubasean! ");
   END IF;
END;

