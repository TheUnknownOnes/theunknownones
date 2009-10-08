//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit PT600;
{F+}
{$i Model.inc}

interface

uses
  WinDos;

type
  PByte= ^Byte;
  PInteger = ^Integer;
  PWord = ^Word;
  TLCD_Bitmap = array [0..31] of byte;
  TFont_SmallLetter = array[0..5] of byte;
  TFont_LargeLetter = array[0..15] of byte;

const
CR			=$0D;
LF			=$0A;

FONT_INT	 =$09;
KERMIT_INT =$0F;
LCD_INT		 =$10;
DOS_INT		 =$21;
POWER_INT	 =$22;
BEEP_INT	 =$31;
RS232_INT	 =$33;

{ comm parameters }
_BAUD_150	=$10;
_BAUD_300	=$20;
_BAUD_600	=$30;
_BAUD_1200	=$40;
_BAUD_2400	=$50;
_BAUD_4800	=$60;
_BAUD_9600	=$70;
_BAUD_19200	=$80;
_BAUD_38400	=$90;
_BAUD_57600	=$A0;
_PAR_NONE	=$00;
_PAR_ODD	=$04;
_PAR_EVEN	=$0C;
_STOP_BIT_1	=$00;
_STOP_BIT_2	=$02;
_DATA_BIT_7	=$00;
_DATA_BIT_8	=$01;


{ function key scan code }
KEY_EXIT	=$84;
KEY_F1		=$86;
KEY_F2		=$87;
KEY_F3		=$88;
KEY_F4		=$89;
KEY_F5		=$8A;
KEY_F6		=$8B;
KEY_F7		=$8C;
KEY_F8		=$8D;
KEY_CLR		  =$08;
KEY_ENTER	  =$0D;
KEY_UP      =$13;
KEY_DOWN    =$12;
KEY_LEFT    =$11;
KEY_RIGHT   =$10;

{ for small font }
{$ifdef PT630}
CHARS_PER_LINE_SMALL	=20;
LINES_PER_SCREEN_SMALL=8;
{$else}
CHARS_PER_LINE_SMALL	=16;
LINES_PER_SCREEN_SMALL=8;
{$endif}

{ for large font }
{$ifdef PT630}
CHARS_PER_LINE_LARGE	=16;
LINES_PER_SCREEN_LARGE=4;
{$else}
CHARS_PER_LINE_LARGE	=12;
LINES_PER_SCREEN_LARGE=4;
{$endif}

{ Display Font Functions }
procedure FONT_large;
procedure FONT_small;
procedure FONT_set(ASize : Byte);
function FONT_get:Byte;
procedure FONT_user_set_all(ASize : Byte; const ABuffer);
procedure FONT_user_get_all(ASize : Byte; const ABuffer);
procedure FONT_user_set_char(ASize : Byte; Ac : Byte; const ABuffer);
procedure FONT_user_get_char(ASize : Byte; Ac : Byte; const ABuffer);

{ Kermit Server Function }
procedure KERMIT_server;


{ LCD Control Functions }
procedure LCD_cls;
procedure LCD_enable_scroll(AFlag : Byte);
procedure LCD_set_cursor(ARow : Byte; AColumn : Byte);
procedure LCD_get_cursor(ARow : PByte; AColumn : PByte);
procedure LCD_display_bitmap_at(ARow, ACol: Byte; const aData: TLCD_Bitmap);
procedure LCD_enable_cursor(AFlag : Byte);
procedure LCD_enable_logo(AFlag : Byte);
procedure LCD_display_char(ACh : Byte);
procedure LCD_display_bitmap(const AData: TLCD_Bitmap);
procedure LCD_Set_Pixel(Ax, Ay, AState : Byte);
function  LCD_Get_Pixel(Ax, Ay :Byte): Byte;


{ System Functions }
procedure SET_backlight(AFlag : Byte);
procedure SET_buzzer(AFlag : Byte);
procedure SET_beeper_volume(Avolume : Byte);
procedure SET_RS232(AFlag : Byte);
procedure SET_key_functions(Akey : Byte; AFlag : Byte);
procedure SET_keypad_language(Alanguage : Byte);
function MAIN_battery_is_low:Byte;
function BACKUP_battery_is_low:Byte;
procedure SET_good_read_LED(AFlag : Byte);
procedure SET_laser_trigger_mode(Amode : Byte);
procedure SET_double_verify(AFlag : Byte);
function SCANNER_is_on:Byte;
function SCANNER_enabled:Byte;
procedure SET_comm_parameter(Apara : Byte);
procedure SET_software_flow_control(ASet : Byte);
procedure SET_hardware_flow_control(ASet : Byte);
procedure SET_comm_protocol(Aprotocol : Byte);
function GET_multipoint_address:Byte;
function SET_multipoint_address(Aaddr : Byte):Byte;
procedure SET_file_upload_msg(ASet : Byte);
procedure SET_terminal_id(const ABuffer);
procedure SET_user_keymap(const ABuffer);
procedure GET_system_keymap(const ABuffer);
procedure SET_barcode_symbology(ACode : Byte; ASet : Byte);
function GET_barcode_status(ACode : Byte):Byte;
procedure SET_code39 (AEnable : Byte; AChkDgt_Vfy : Byte; ASend_ChkDgt : Byte; ASend_StartStop : Byte; AFull_Ascii : Byte);
procedure SET_I2of5 (AEnable : Byte; AChkDgt_Vfy : Byte; ASend_ChkDgt : Byte);
procedure SET_codabar (AEnable : Byte; AChkDgt_Vfy : Byte; ASend_ChkDgt : Byte; ASend_StartStop : Byte);
procedure SET_code128 (AEnable : Byte);
procedure SET_EAN128 (AEnable : Byte);
procedure SET_code93 (AEnable : Byte);
procedure SET_UPC_A (AEnable : Byte; ASend_ChkDgt : Byte; ASend_LeadDgt : Byte);
procedure SET_UPC_E (AEnable : Byte; ASend_ChkDgt : Byte; ASend_LeadDgt : Byte; Azero_expan : Byte);
procedure SET_EAN13 (AEnable : Byte; ASend_ChkDgt : Byte; ASend_LeadDgt : Byte);
procedure SET_EAN8 (AEnable : Byte; ASend_ChkDgt : Byte);
procedure SET_TRIOPTIC (AEnable : Byte);
procedure SET_TOSHIBA (AEnable : Byte);
procedure GET_system_date(var AYear : Word; var AMonth,ADay,AWeekday : Byte);
procedure SET_system_date(AYear : Word; AMonth : Byte; ADay : Byte);
procedure GET_system_time(var AHour,AMinute,ASecond,AhSecond : Byte);
procedure SET_system_time(AHour : Byte; AMinute : Byte; ASecond : Byte);
procedure SET_alarm_date(ASet : Byte; Ayear : Word; AMonth : Byte; ADay : Byte);
procedure SET_alarm_time(AHour : Byte; AMinute : Byte; ASecond : Byte);
procedure GET_version_number(var ADOS_ver,AFW_Ver,AOEM : Integer);
procedure GET_free_disk(var ATotal_RAM,AFree_RAM : Integer);
function Read_Scanner_Port(const Buffer; var ACode, ADir: byte): Boolean;
procedure SET_scanner_port(ASet : Byte);
function READ_multipoint_data(const ABuffer):Boolean;
function SEND_multipoint_data(const ABuffer):Boolean;
function BUFFER_is_empty:Boolean;
procedure Delete_File(const AFileName);
procedure Rename_File(const AFromFileName,AToFileName);
procedure SET_Keypad_Numeric(ANumeric : Boolean);


{ Power Management Function }
function POWER_saving_mode:Byte;


{ Beeper Control Function }
procedure BEEP_control(Afreq : Word; Aduration : Word);


{ RS232 Control Functions }
procedure RS232_set_comm_para(APara : Byte);
function RS232_get_char(var ACh : Byte):Boolean;
procedure RS232_send_char(ACh : Byte);
procedure RS232_enable;
procedure RS232_disable;
procedure RS232_set_RTS_DTR(Asignal : Byte; ASet : Byte);
function RS232_get_CTS:Byte;

{ Key functions }
function KeyPressed: Boolean;
function ReadKey:Byte;

implementation
{##############################################################################
                              Implementation
 ##############################################################################}

{-----------------------------------------------------------------------
  Display Font Functions (INT 09H)
-------------------------------------------------------------------------}

{ Select large font }
procedure FONT_large;
var
  regs: TRegisters;
begin
  regs.ah:=0;
  intr(FONT_INT,regs);
end;

{ Select small font }
procedure FONT_small;
var
  regs: TRegisters;
begin
  regs.ah:=1;
  intr(FONT_INT,regs);
end;

{ Set font type (0=large,1=small) }
procedure FONT_set(ASize : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=2;
  regs.al:=ASize;
  intr(FONT_INT,regs);
end;

{ Get font type (0=large,1=small) }
function FONT_get:Byte;
var
  regs: TRegisters;
begin
  regs.ah:=3;
  intr(FONT_INT,regs);
  FONT_get:=regs.al;
end;

{ Set user-defined font for all characters (0=large,1=small) }
procedure FONT_user_set_all(ASize : Byte; const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, 4;
    mov al, ASize;
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int FONT_INT;
    pop ds;
  end;
end;

{ Get font data for all characters (0=large,1=small) }
procedure FONT_user_get_all(ASize : Byte; const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, 5;
    mov al, ASize;
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int FONT_INT;
    pop ds;
  end;
end;

{ Set user-defined font for one character (0=large,1=small) }
procedure FONT_user_set_char(ASize : Byte; Ac : Byte; const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, 6;
    mov al, ASize;
    mov cl, AC
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int FONT_INT;
    pop ds;
  end;
end;

{ Get font data for one character (0=large,1=small) }
procedure FONT_user_get_char(ASize : Byte; Ac : Byte; const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, 7;
    mov al, ASize;
    mov cl, AC
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int FONT_INT;
    pop ds;
  end;
end;


{-----------------------------------------------------------------------
  Kermit Server Function (INT 0FH)
-------------------------------------------------------------------------}

{ a funny function without comment}
procedure KERMIT_server;
var
  regs: TRegisters;
begin
  intr(KERMIT_INT,regs);
end;


{-----------------------------------------------------------------------
  LCD Control Functions (INT 10H)
-------------------------------------------------------------------------}

{ Clear LCD screen }
procedure LCD_cls;
var
  regs: TRegisters;
begin
  regs.ah:=0;
  intr(LCD_INT,regs);
end;

{ Enable/Disable screen scroll (0=disable,1=enable) }
procedure LCD_enable_scroll(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=1;
  regs.al:=AFlag;
  intr(LCD_INT,regs);
end;

{ Set cursor position }
procedure LCD_set_cursor(ARow : Byte; AColumn : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=2;
  regs.dh:=ARow;
  regs.dl:=AColumn;
  intr(LCD_INT,regs);
end;

{ Get cursor position }
procedure LCD_get_cursor(ARow : PByte; AColumn : PByte);
var
  regs: TRegisters;
begin
  regs.ah:=3;
  intr(LCD_INT,regs);
  ARow^:=regs.dh;
  AColumn^:=regs.dl;
end;

procedure LCD_display_bitmap_at(ARow, ACol: Byte; const aData: TLCD_Bitmap);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(aData);
  aOfs:=ofs(aData);
  asm
    mov ah, $4;
    mov dh, aRow;
    mov dl, aCol;
    push ds;
    mov ds,aSeg;
    mov bx,aOfs;
    int LCD_INT;
    pop ds;
  end;
end;

{ Set cursor on/off (0=off,1=on) }
procedure LCD_enable_cursor(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=5;
  regs.al:=AFlag;
  intr(LCD_INT,regs);
end;

{ Enable/Disable power-on logo display }
procedure LCD_enable_logo(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=5;
  regs.al:=AFlag;
  intr(LCD_INT,regs);
end;

{ Display character }
procedure LCD_display_char(ACh : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$a;
  regs.al:=ACh;
  intr(LCD_INT,regs);
end;

{ Display 16*16 bitmap }
procedure LCD_display_bitmap(const AData: TLCD_Bitmap);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(AData);
  aOfs:=ofs(AData);
  asm
    mov ah, $4f;
    push ds;
    mov ds,aSeg;
    mov bx,aOfs;
    int LCD_INT;
    pop ds;
  end;
end;

{ Set a pixel (0=no pixel, 1=pixel)}
procedure LCD_Set_Pixel(Ax, Ay, AState : Byte);
var
  regs : TRegisters;
begin
  regs.ah:=$41;
  regs.al:=AState;
  regs.dl:=Ax;
  regs.dh:=Ay;
  intr(LCD_INT, regs);
end;

{ Get a pixel (0=no pixel, 1=pixel)}
function LCD_Get_Pixel(Ax, Ay :Byte): Byte;
var
  regs : TRegisters;
begin
  regs.ah:=$40;
  regs.dl:=Ax;
  regs.dh:=Ay;
  intr(LCD_INT, regs);
  LCD_Get_Pixel:=regs.al;
end;


{-----------------------------------------------------------------------
  System Functions (INT 21H)
-------------------------------------------------------------------------}

{ LCD backlight on/off control (0=off,1=on) }
procedure SET_backlight(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$0;
  regs.al:=AFlag;
  intr(DOS_INT,regs);
end;

{ Buzzer on/off control }
procedure SET_buzzer(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$1;
  regs.al:=AFlag;
  intr(DOS_INT,regs);
end;

{ Beeper volume (0=low,1=medium,2=high) }
procedure SET_beeper_volume(AVolume : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$3;
  regs.al:=AVolume;
  intr(DOS_INT,regs);
end;

{ Enable/Disable RS232 port (0=disable,1=enable) }
procedure SET_RS232(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$4;
  regs.al:=AFlag;
  intr(DOS_INT,regs);
end;

{ Enable/Disable key or key function (see technical binder) }
procedure SET_key_functions(Akey : Byte; AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$5;
  regs.al:=AKey;
  regs.bl:=AFlag;
  intr(DOS_INT,regs);
end;

{ Set keypad language }
procedure SET_keypad_language(Alanguage : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$7;
  regs.al:=ALanguage;
  intr(DOS_INT,regs);
end;

{ Check main battery status (0=normal,1=low) }
function MAIN_battery_is_low:Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$8;
  intr(DOS_INT,regs);
  MAIN_battery_is_low:=regs.al;
end;

{ Check backup battery status (0=normal,1=low) }
function BACKUP_battery_is_low:Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$9;
  intr(DOS_INT,regs);
  BACKUP_battery_is_low:=regs.al;
end;

{ Set good-read LED (0=off,1=on,2=control by system) }
procedure SET_good_read_LED(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$a;
  regs.al:=AFlag;
  intr(DOS_INT,regs);
end;

{ Set laser scanner trigger mode (0=normal,1=flash) }
procedure SET_laser_trigger_mode(Amode : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$b;
  regs.al:=AMode;
  intr(DOS_INT,regs);
end;

{ Enable/Disable double verification }
procedure SET_double_verify(AFlag : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$c;
  regs.al:=AFlag;
  intr(DOS_INT,regs);
end;

{ Check laser scanner (0=no scanner,1=yes) }
function SCANNER_is_on:Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$d;
  intr(DOS_INT,regs);
  SCANNER_is_on:=regs.al;
end;

{ Check scanner port status (0=disabled,1=enabled) }
function SCANNER_enabled:Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1b;
  regs.bh:=$5;
  intr(DOS_INT,regs);
  SCANNER_enabled:=regs.al;
end;

{ Set communication parameters }
procedure SET_comm_parameter(Apara : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$1;
  regs.al:=APara;
  intr(DOS_INT,regs);
end;

{ Software control flow (0=enable XON/XOFF, 1=disable XON/XOFF) }
procedure SET_software_flow_control(ASet : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$2;
  regs.al:=ASet;
  intr(DOS_INT,regs);
end;

{ Hardware control flow (0=enable CTS/RTS, 1=disable CTS/RTS) }
procedure SET_hardware_flow_control(ASet : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$3;
  regs.al:=ASet;
  intr(DOS_INT,regs);
end;

{ Set communication protocol (2=multipoint, 3=none) }
procedure SET_comm_protocol(Aprotocol : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$4;
  regs.al:=AProtocol;
  intr(DOS_INT,regs);
end;

{ Get multipoint address }
function GET_multipoint_address:Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$5;
  intr(DOS_INT,regs);
  GET_multipoint_address:=regs.al;
end;

{ Set multipoint address (0=success,1=fail) }
function SET_multipoint_address(Aaddr : Byte):Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$6;
  regs.al:=AAddr;
  intr(DOS_INT,regs);
  SET_multipoint_address:=regs.al;
end;

{ Set file uploading message on/off }
procedure SET_file_upload_msg(ASet : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1c;
  regs.bh:=$a;
  regs.al:=ASet;
  intr(DOS_INT,regs);
end;

{ Set terminal ID }
procedure SET_terminal_id(const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, $1d;
    mov bh, $0
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int DOS_INT;
    pop ds;
  end;
end;

{ Set user-defined keymap }
procedure SET_user_keymap(const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, $1e;
    mov al, $0
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int DOS_INT;
    pop ds;
  end;
end;

{ Get system keymap }
procedure GET_system_keymap(const ABuffer);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, $1e;
    mov al, $1
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int DOS_INT;
    pop ds;
  end;
end;

{ Enable/Disable decoding of a barcode symbology
 (0=all, 1=code 39, 2=I2 of 5, 3=codebar, 4=EAN/UPC, 5=code 128, 6=EAN 128) }
procedure SET_barcode_symbology(ACode : Byte; ASet : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$1;
  regs.bl:=ACode;
  regs.al:=ASet;
  intr(DOS_INT,regs);
end;

{ Get decoding status of a barcode symbology }
function GET_barcode_status(ACode : Byte):Byte;
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$2;
  regs.bl:=ACode;
  intr(DOS_INT,regs);
  GET_barcode_status:=regs.al;
end;

{ Code 39 settings }
procedure SET_code39 (AEnable : Byte; AChkDgt_Vfy : Byte; ASend_ChkDgt : Byte;          
                      ASend_StartStop : Byte; AFull_Ascii : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$1;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (AChkDgt_Vfy=1) then
    regs.al:=regs.al or $2
  else
    regs.al:=regs.al and $fd;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  if (ASend_StartStop=1) then
    regs.al:=regs.al or $8
  else
    regs.al:=regs.al and $f7;
  if (AFull_Ascii=1) then
    regs.al:=regs.al or $10
  else
    regs.al:=regs.al and $ef;
  intr(DOS_INT,regs);
end;

{ I 2 of 5 settings }
procedure SET_I2of5 (AEnable : Byte; AChkDgt_Vfy : Byte; ASend_ChkDgt : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$2;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (AChkDgt_Vfy=1) then
    regs.al:=regs.al or $2
  else
    regs.al:=regs.al and $fd;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  intr(DOS_INT,regs);
end;

{ Codabar settings }
procedure SET_codabar (AEnable : Byte; AChkDgt_Vfy : Byte; ASend_ChkDgt : Byte; 
                        ASend_StartStop : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$3;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (AChkDgt_Vfy=1) then
    regs.al:=regs.al or $2
  else
    regs.al:=regs.al and $fd;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  if (ASend_StartStop=1) then
    regs.al:=regs.al or $8
  else
    regs.al:=regs.al and $f7;
  intr(DOS_INT,regs);
end;

{ Code 128 settings }
procedure SET_code128 (AEnable : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$5;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  intr(DOS_INT,regs);
end;

{ EAN 128 settings }
procedure SET_EAN128 (AEnable : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$6;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  intr(DOS_INT,regs);
end;

{ Code 93 settings }
procedure SET_code93 (AEnable : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$7;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  intr(DOS_INT,regs);
end;

{ UPC-A settings }
procedure SET_UPC_A (AEnable : Byte; ASend_ChkDgt : Byte; ASend_LeadDgt : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$11;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  if (ASend_LeadDgt=1) then
    regs.al:=regs.al or $8
  else
    regs.al:=regs.al and $f7;
  intr(DOS_INT,regs);
end;

{ UPC-E settings }
procedure SET_UPC_E (AEnable : Byte; ASend_ChkDgt : Byte; ASend_LeadDgt : Byte; 
                      AZero_Expan : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$12;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  if (ASend_LeadDgt=1) then
    regs.al:=regs.al or $8
  else
    regs.al:=regs.al and $f7;
  if (AZero_Expan=1) then
    regs.al:=regs.al or $10
  else
    regs.al:=regs.al and $ef;
  intr(DOS_INT,regs);
end;

{ EAN-13 settings }
procedure SET_EAN13 (AEnable : Byte; ASend_ChkDgt : Byte; ASend_LeadDgt : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$13;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  if (ASend_LeadDgt=1) then
    regs.al:=regs.al or $8
  else
    regs.al:=regs.al and $f7;
  intr(DOS_INT,regs);
end;

{ EAN-8 settings }
procedure SET_EAN8 (AEnable : Byte; ASend_ChkDgt : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$14;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  if (ASend_ChkDgt=1) then
    regs.al:=regs.al or $4
  else
    regs.al:=regs.al and $fb;
  intr(DOS_INT,regs);
end;

{ TRIOPTIC code settings }
procedure SET_TRIOPTIC (AEnable : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$21;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  intr(DOS_INT,regs);
end;

{ TOSHIBA code settings }
procedure SET_TOSHIBA (AEnable : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$1f;
  regs.bh:=$3;
  regs.bl:=$22;
  regs.al:=$0;
  if (AEnable=1) then
    regs.al:=regs.al or $1
  else
    regs.al:=regs.al and $fe;
  intr(DOS_INT,regs);
end;

{ Get system date }
procedure GET_system_date(var AYear : Word; var AMonth,ADay,AWeekday : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$2a;
  intr(DOS_INT,regs);
  AYear:=regs.cx;
  AMonth:=regs.dh;
  ADay:=regs.dl;
  AWeekday:=regs.al;
end;

{ Set system date }
procedure SET_system_date(AYear : Word; AMonth : Byte; ADay : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$2b;
  regs.cx:=AYear;
  regs.dh:=AMonth;
  regs.dl:=ADay;
  intr(DOS_INT,regs);
end;

{ Get system time }
procedure GET_system_time(var AHour,AMinute,ASecond,AhSecond : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$2c;
  intr(DOS_INT,regs);
  AHour:=regs.ch;
  AMinute:=regs.cl;
  ASecond:=regs.dh;
  AhSecond:=regs.dl;
end;

{ Set system time }
procedure SET_system_time(AHour : Byte; AMinute : Byte; ASecond : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$2d;
  regs.ch:=AHour;
  regs.cl:=AMinute;
  regs.dh:=ASecond;
  intr(DOS_INT,regs);
end;

{ Set alarm date (0=disable, 1=enable everyday, 2=enable by date) }
procedure SET_alarm_date(ASet : Byte; Ayear : Word; AMonth : Byte; ADay : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$2e;
  regs.al:=ASet;
  if (ASet=2) then
  begin
    regs.cx:=Ayear;
    regs.dh:=AMonth;
    regs.dl:=ADay;
  end;
  intr(DOS_INT,regs);
end;

{ Set alarm time }
procedure SET_alarm_time(AHour : Byte; AMinute : Byte; ASecond : Byte);
var
  regs: TRegisters;
begin
  regs.ah:=$2f;
  regs.ch:=AHour;
  regs.cl:=AMinute;
  regs.dh:=ASecond;
  intr(DOS_INT,regs);
end;

{ Get PT600 DOS and firmware version number }
procedure GET_version_number(var ADOS_ver,AFW_Ver,AOEM : Integer);
var
  regs: TRegisters;
begin
  regs.ah:=$30;
  regs.al:=0;
  intr(DOS_INT,regs);
  ADOS_ver:=regs.ax;
  AFW_Ver:=regs.cx;
  AOEM:=regs.bx;
end;

{ Get free disk cluster }
procedure GET_free_disk(var ATotal_RAM,AFree_RAM : Integer);
var
  regs: TRegisters;
begin
  regs.ah:=$36;
  intr(DOS_INT,regs);
  ATotal_RAM:=regs.dx;
  AFree_RAM:=regs.bx;
end;


{ Read data from scanner port (TRUE=has data, FALSE=no data)
 	  code: 1 code39; 2 I2of5; 3 Codabar; 5 Code128 6 EAN128 7 Code93
	       11H UPC-A; 12H UPC-E; 13H EAN-13; 14H EAN-8; 21H Trioptic; 22H Toshiba
    dir: 0 from left to right; 1 from right to left }
function Read_Scanner_Port(const Buffer; var ACode, ADir: Byte): Boolean;
var
  blv   : byte;
  clv   : byte;
  axv   : word;
  aSeg  : Word;
  aOfs  : Word;
begin
  aSeg:=seg(Buffer);
  aOfs:=ofs(Buffer);
  asm
    mov ah, $50;
    push ds;
    mov dx,aOfs;
    mov ds,aSeg;
    int DOS_INT;
    pop ds;
    mov [axv], ax;
    mov [blv], bl;
    mov [clv], cl;
  end;
  acode:=blv;
  adir:=clv;
  read_scanner_port:=axv=0;
end;

{ Set scanner port (0=disable, 1=enable) }
procedure SET_scanner_port(ASet : Byte);
var
  regs : TRegisters;
begin
  regs.ah:=$51;
  regs.al:=aSet;
  intr(DOS_INT, regs);
end;

{ Receive data from RS232 port in multipoint protocol }
function READ_multipoint_data(const ABuffer):Boolean;
var
  aSeg : Word;
  aOfs : Word;
  Res  : Byte;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, $5c;
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int DOS_INT;
    mov Res,al;
    pop ds;
  end;
  if (Res=0) then
    READ_multipoint_data:=true
  else
    READ_multipoint_data:=false;
end;

{ Send data to RS232 port in multipoint protocol }
function SEND_multipoint_data(const ABuffer):Boolean;
var
  aSeg : Word;
  aOfs : Word;
  Res  : Byte;
begin
  aSeg:=seg(ABuffer);
  aOfs:=ofs(ABuffer);
  asm
    mov ah, $5d;
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int DOS_INT;
    mov Res,al;
    pop ds;
  end;
  if (Res=0) then
    SEND_multipoint_data:=true
  else
    SEND_multipoint_data:=false;
end;

{ Check RS232 output buffer status in multipoint protocol }
function BUFFER_is_empty:Boolean;
var
  regs : TRegisters;
begin
  regs.ah:=$5e;
  intr(DOS_INT, regs);
  if (regs.al=0) then
    BUFFER_is_empty:=true
  else
    BUFFER_is_empty:=false;
end;

{ Delete the specified File}
procedure Delete_File(const AFileName);
var
  aSeg : Word;
  aOfs : Word;
begin
  aSeg:=seg(AFileName);
  aOfs:=ofs(AFileName);
  asm
    mov ah, $41;
    push ds;
    mov ds,aSeg;
    mov dx,aOfs;
    int DOS_INT;
    pop ds;
  end;
end;

procedure Rename_File(const AFromFileName,AToFileName);
var
  aFSeg : Word;
  aFOfs : Word;
  aTSeg : Word;
  aTOfs : Word;
begin
  aFSeg:=seg(AFromFileName);
  aFOfs:=ofs(AFromFileName);
  aTSeg:=seg(AToFileName);
  aTOfs:=ofs(AToFileName);
  asm
    mov ah, $56;
    push ds;
    mov ds,aFSeg;
    mov dx,aFOfs;
    mov es,aTSeg;
    mov di,aTOfs;
    int DOS_INT;
    pop ds;
  end;
end;

procedure SET_Keypad_Numeric(ANumeric : Boolean);
var
  regs : TRegisters;
begin
  regs.ah:=$1a;
  regs.bh:=$12;
  if (not ANumeric) then
    regs.al:=1
  else
    regs.al:=0;
  intr(DOS_INT, regs);
end;

{-----------------------------------------------------------------------
  Power Management Function (INT 22H)
-------------------------------------------------------------------------}

{ Power management function }
function POWER_saving_mode:Byte;
var
  regs : TRegisters;
begin
  intr(POWER_INT, regs);
  POWER_saving_mode:=regs.ax;
end;

{-----------------------------------------------------------------------
  Beeper Control Function (INT 31H)
-------------------------------------------------------------------------}

procedure BEEP_control(Afreq : Word; Aduration : Word);
var
  regs : TRegisters;
begin
  regs.ax:=AFreq;
  regs.bx:=ADuration;
  intr(BEEP_INT, regs);
end;

{-----------------------------------------------------------------------
  RS232 Control Functions (INT 33H)
-------------------------------------------------------------------------}

{ Set communication parameters }
procedure RS232_set_comm_para(APara : Byte);
var
  regs : TRegisters;
begin
  regs.ah:=$0;
  regs.al:=APara;
  intr(RS232_INT, regs);
end;

{ Get character from RS232 port }
function RS232_get_char(var ACh : Byte):Boolean;
var
  regs : TRegisters;
begin
  regs.ah:=$1;
  intr(RS232_INT, regs);
  if (regs.ah=0) then
  begin
    ACh:=regs.al;
    RS232_get_char:=true;
  end
  else
    RS232_get_char:=false;
end;

{ Send character to RS232 port }
procedure RS232_send_char(ACh : Byte);
var
  regs : TRegisters;
begin
  regs.ah:=$2;
  regs.al:=ACh;
  intr(RS232_INT, regs);
end;

{ Enable RS232 port}
procedure RS232_enable;
var
  regs : TRegisters;
begin
  regs.ah:=$3;
  intr(RS232_INT, regs);
end;

{ Disable RS232 port }
procedure RS232_disable;
var
  regs : TRegisters;
begin
  regs.ah:=$4;
  intr(RS232_INT, regs);
end;

{ Set RTS/DTR signal of RS232 port (1=DTR,2=RTS; 0=enable,1=disable) }
procedure RS232_set_RTS_DTR(Asignal : Byte; ASet : Byte);
var
  regs : TRegisters;
begin
  regs.ah:=$5;
  regs.al:=ASignal;
  regs.dh:=ASet;
  intr(RS232_INT, regs);
end;

{ Get CTS signal status of RS232 port }
function RS232_get_CTS:Byte;
var
  regs : TRegisters;
begin
  regs.ah:=$6;
  regs.al:=$1;
  intr(RS232_INT, regs);
  RS232_get_CTS:=regs.dh;
end;

{-----------------------------------------------------------------------
  Key functions
-------------------------------------------------------------------------}
function KeyPressed: Boolean;
var
  regs : TRegisters;
begin
  regs.ah:=$0b;
  intr(DOS_INT, regs);
  KeyPressed:=regs.al<>0;
end;

function ReadKey:Byte;
var
  regs : TRegisters;
begin
  regs.ah:=$7;
  intr(DOS_INT, regs);
  ReadKey:=regs.al;
end;

end.
