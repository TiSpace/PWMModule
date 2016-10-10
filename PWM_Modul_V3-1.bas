' * ---------------------------------------------------------
' *            PWM Erzeugung
' *            Auswahl manuell oder remote
' *
' *            Serielle Kommunikation
' *            AD Eingang
' *            GPIO Eingang
' *            2x PWM Ausgang
' *
' *            Pinbelegung:
' *            3 (PB4) - Tx
' *            2 (PB3) - Rx und alternativ ADC zum Schalter lesen       (ADC3)
' *            7 (PB2) - PWM manuell (ADC Poti)                         (ADC1)
' *            5 (PB0) - PWM Kanal 1  OC0A: Timer/Counter0 Compare Match A output
' *            6 (PB1) - PWM Kanal 2  OC0B: Timer/Counter0 Compare Match B Output
' *
' *            Hardware: tr090-2016 R1
' *            Datum:    Juli 2016
' */

' *  Historie:
' *  V1: funktional
' *  V2: Code optimiert
' *  V3: Zwei-Wege-Kommunikation für Parameterübergabe
' *      Setting in EEPROM speichern nach Änderung via PC
' *  V3.1: Debugging

' ------------------------- Chipauswahl ----------------------------------------
$regfile = "attiny45.dat"
$crystal = 8e6
$hwstack = 32
$swstack = 32
$framesize = 32

' ------------------------- Konstanten -----------------------------------------

   const Timerkonstante = 131                               ' für 1ms Ticker

' ------------------------- Funktionen -----------------------------------------

   declare sub HoleByte() as byte


' ------------------------- Variablen   ----------------------------------------


Dim Poti as word                                            ' ADC zum Lesen der Potistellung
dim Fkt_Wahl as word                                        ' ADC zum Lesen der Schalterstellung
DIM Funktion as Byte
dim Kanal as byte
dim i as Byte

dim Data_income as byte                                     'ankommendes Byte
dim WaitCounter as Word

dim PWM_A as eram byte , PWM_B as eram byte , Freq as eram byte,

' ------------------------- Initialisierung  -----------------------------------

   ' Timer für PWM
   Config Timer0 = Pwm , Compare A Pwm = Clear Up , Compare B Pwm = Clear Up , Prescale = 256
   OCR0A = 30
   OCR0B = 30

   ' Timer für 1ms Ticker
   Config Timer1 = Timer , Prescale = 64
   On Ovf1 Tim1_isr
   Tcnt1 = Timerkonstante

   ' SW Seriellport
   Open "COMB.4:9600,8,N,1" For Output As #1
   Open "COMB.3:9600,8,N,1" For Input As #2

   ' ADC Kanäle
   Config Adc = Single , Prescaler = Auto
   start adc

   enable Timer1
   enable interrupts



' ------------------------- Hauptprogramm  -------------------------------------
print #1 , Version(3)                                       ' Programmname ausgeben

' hole gespeicherte Setting aus EEPROM
     OCR0A = PWM_A
     OCR0B = PWM_B
 ' print #1, "GTCCR: ";GTCCR
 ' print #1,"TCCR0A: ";TCCR0A
 ' print #1,"TCCR0B: ";TCCR0B

     TCCR0B = Freq
     TCCR0B = TCCR0B and &h07  ' nur CS0..CS2 dürfen für die richtige Betriebsart gesetzt sein

Do




   ' gewählte Funktion prüfen
   Fkt_Wahl = getadc(3)
      ' PWM1 über Poti einstellen ADC~176
      ' PWM2 über Poti einstellen ADC~592
      ' 3.3V  = Remotefunktion
   'print #1, Fkt_Wahl;"  ";
   'Poti = getadc(1)
   'print #1, Poti

      if Fkt_Wahl > 150 and Fkt_Wahl < 200 then
         Funktion = 1
         Kanal = 1
      elseif Fkt_Wahl > 560 and Fkt_Wahl < 620 then
        Funktion = 2
        Kanal = 2
      else
         Funktion = 3
      end if




      if Funktion < 3 then                                  ' direkter Potimodus
         Poti = getadc(1)
         Poti = Poti / 4                                    'auf 8Bit trimmen
      else
         ' Byte 1: Daten werden gesendet, Wert egal
         ' Byte 2: Kanalnummer (gültig sind 1 oder 2)
         '         oder PWM Frequenz (3)
         ' Byte 3: Setting

         Data_income = inkey(#2)
         if Data_income <> 0 then                           ' wird ein Byte (1) empfangen?

            print #1 , "Attn " ; Data_income

              call HoleByte()


              if Funktion > 0 then                          ' wurde Byte 2 empfangen? wenn ja, dann weiter beim Empfang
                 select case Data_income
                        case 49                             ' = 1
                             Kanal = 1
                        case 50                             ' "2"
                             Kanal = 2
                        case 51                             ' "3"
                             Kanal = 3                      'Frequenz festlegen
                        case 52                             ' "4"        Rückgabe der verschiedenen Parameterübergabe
                             print #1 , OCR0A               'PWM1
                             print #1 , OCR0B               'PWM2
                             print #1 , TCCR0B              'Prescaler
                             Kanal = 0
                        case else
                            Kanal = 0
                 end select



                 if Kanal <> 0 then                         ' die Vorwahl muss gültig getroffen worden sein
                    call HoleByte()

                    if Funktion > 0 then
                       Poti = Data_income
                    end if
                 end if

              end if
            end if
      end if



         ' empfangene Daten auswerten -> Tastverhältnis
         select case Funktion
            case 0
                 ' nichts machen, Eingabe war ungültig
            case 1
               OCR0A = Poti
              ' print #1, "Poti 1 gesetzt ";Poti;"   f:";TCCR0B
            case 2
               OCR0B = Poti
              ' print #1, "Poti 2 gesetzt ";Poti;"   f:";TCCR0B
            case 3
                 select case Kanal
                      case 0
                           ' ungültiger Wert
                      case 1
                           OCR0A = Poti
                           PWM_A = Poti                     'in EEPROM
                           print #1 , "A gesetzt: " ; Poti
                      case 2
                           OCR0B = Poti
                           PWM_B = Poti                     'in EEPROM
                           print #1 , "B gesetzt: " ; Poti
                 end select


         end select

         ' empfangene Daten auswerten -> Frequenz
         if Kanal = 3 then                                  ' PWM Freq festlegen
            Data_income = Data_income - 48                  ' von ASCI auf BYTE umrechenne

            ' gültige Werte für Data_income sind 1..5
            if Data_income > 0 and Data_income < 6 then
                i = TCCR0B                                  ' hole Register
                i = i and &hf8                              ' untere drei Bit ausmaskieren

                TCCR0B = i or Data_income                   ' untere drei Bit entsprechend setzten
                Freq = TCCR0B
                print #1 , "Freq gesetzt: " ; TCCR0B
            end if
         end if

         Kanal = 0 : Funktion = 0                           ' zurücksetzten

Loop






Close #2

Close #1
 ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Subroutinen - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sub HoleByte() as byte

         WaitCounter = 0 : Data_income = 0                  'Startwerte rücksetzen
         while Data_income = 0
               Data_income = inkey(#2)
               if Data_income <> 0 then
                  print #1 , "Byte: " ; Data_income

               end if
               if WaitCounter > 2000 then                   ' max 2sek für Eingabe zulassen
                  Funktion = 0
                  print #1 , "Eingabe abgebrochen"
                  exit while
               end if
         wend

end sub




' ---------------------------------- ISR Routinen ------------------------------
Tim1_isr:
   Tcnt1 = Timerkonstante
   incr WaitCounter
return



End