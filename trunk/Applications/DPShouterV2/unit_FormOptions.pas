unit unit_FormOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, ExtCtrls, JclFileUtils;

type
  Tform_Options = class(TForm)
    gb_Alpha: TGroupBox;
    tb_AlphaActive: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    tb_AlphaInactive: TTrackBar;
    Label4: TLabel;
    gb_Optionen: TGroupBox;
    cb_ShowNewPosts: TCheckBox;
    cb_StayOnTop: TCheckBox;
    cb_HighlightMe: TCheckBox;
    pan_Bottom: TPanel;
    btn_Ok: TBitBtn;
    btn_Cancel: TBitBtn;
    btn_DelCache: TBitBtn;
    Label5: TLabel;
    tb_Fading: TTrackBar;
    Label6: TLabel;
    Label7: TLabel;
    procedure btn_DelCacheClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_Options: Tform_Options;

implementation

uses unit_Data;

{$R *.dfm}

procedure Tform_Options.btn_DelCacheClick(Sender: TObject);
var
  files : TStrings;
begin
  files:=TStringList.Create;
  BuildFileList(Data.Dir_Cache+'*.*',faAnyFile,files);
  while files.Count>0 do
  begin
    DeleteFile(Data.Dir_Cache+files[0]);
    files.Delete(0);
  end;
  files.Free;
end;

procedure Tform_Options.FormShow(Sender: TObject);
begin
  Self.Color:=COLOR_Form;
end;

end.
