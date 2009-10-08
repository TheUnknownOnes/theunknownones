unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, ComCtrls, uEffectPNGToolbar, ToolWin, StdCtrls, dEXIF,
  ExtCtrls, xpman, ShellAPI;

type
  Tform_Main = class(TForm)
    gb_Files: TGroupBox;
    lv_Files: TListView;
    EffectPNGToolBar1: TEffectPNGToolBar;
    btn_AddFile: TEffectPNGToolButton;
    btn_Remove: TEffectPNGToolButton;
    dlg_Picture: TOpenPictureDialog;
    pan_Bottom: TPanel;
    Label1: TLabel;
    PB: TProgressBar;
    ToolButton1: TToolButton;
    btn_Rename: TEffectPNGToolButton;
    gb_Options: TGroupBox;
    Label2: TLabel;
    ed_NamingRule: TEdit;
    Label3: TLabel;
    procedure btn_AddFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_RemoveClick(Sender: TObject);
    procedure lv_FilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btn_RenameClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    FImgData : TImgData;
    procedure AddFileToList(AFileName : String);
  public
    { Public-Deklarationen }
  end;

var
  form_Main: Tform_Main;

implementation

{$R *.dfm}

procedure Tform_Main.AddFileToList(AFileName: String);
begin
  if FImgData.ProcessFile(AFileName) and FImgData.HasEXIF then
  begin
    if lv_Files.FindCaption(0, AFileName, false, true, false) = nil then
    with lv_Files.Items.Add do
    begin
      Caption := AFileName;
      SubItems.Add(DateTimeToStr(FImgData.ExifObj.GetImgDateTime));
    end
  end;

  
end;

procedure Tform_Main.btn_AddFileClick(Sender: TObject);
begin
  if dlg_Picture.Execute then
  begin
    PB.Max := dlg_Picture.Files.Count;
    pb.Min := 0;
    pb.Position := 0;

    try
      while dlg_Picture.Files.Count > 0 do
      begin
        AddFileToList(dlg_Picture.Files[0]);
        dlg_Picture.Files.Delete(0);

        pb.Position := pb.Position + 1;
        pb.Repaint;
      end;
    finally
      pb.Position := pb.Min;
    end;
  end;
end;

procedure Tform_Main.btn_RemoveClick(Sender: TObject);
begin
  lv_Files.DeleteSelected;
end;

procedure Tform_Main.btn_RenameClick(Sender: TObject);
var
  idx : Integer;
  OrigFileName,
  NewFileName : String;
begin
  pb.Min := 0;
  pb.Position := 0;
  pb.Max := lv_Files.Items.Count;

  try
    for idx  := 0 to lv_Files.Items.Count - 1 do
    begin
      OrigFileName := lv_Files.Items[idx].Caption;

      FImgData.ProcessFile(OrigFileName);

      NewFileName := IncludeTrailingPathDelimiter(ExtractFilePath(OrigFileName)) +
                     FormatDateTime(ed_NamingRule.Text,FImgData.ExifObj.GetImgDateTime) +
                     ExtractFileExt(OrigFileName);

      if MoveFile(PChar(OrigFileName), Pchar(NewFileName)) then
        lv_Files.Items[idx].Caption := NewFileName;

      pb.Position := idx;
      pb.Repaint;
    end;
  finally
    pb.Position := 0;
  end;
end;

procedure Tform_Main.FormCreate(Sender: TObject);
begin
  FImgData := TimgData.Create();
  FImgData.BuildList := GenAll;
end;

procedure Tform_Main.FormDestroy(Sender: TObject);
begin
  FImgData.Free;
end;

procedure Tform_Main.Label3Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.delphibasics.co.uk/RTL.asp?Name=FormatDateTime', nil, nil, SW_SHOWNORMAL);
end;

procedure Tform_Main.lv_FilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btn_Remove.Click;
end;

end.
