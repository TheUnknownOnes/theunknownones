//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unit_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NPipe_Client,NPipe_Types, ComCtrls;

type
  TForm4 = class(TForm)
    NPipeClient1: TNPipeClient;
    ed_Pipename: TLabeledEdit;
    ed_Pipeserver: TLabeledEdit;
    lbl_SendTimeout: TLabel;
    tb_SendTimeout: TTrackBar;
    btn_SendFile: TButton;
    ed_FileName: TLabeledEdit;
    mem_Data: TMemo;
    btn_SendText: TButton;
    gb_Progress: TGroupBox;
    PB: TProgressBar;
    gb_Reply: TGroupBox;
    rb_ReplyToFile: TRadioButton;
    ed_SaveFileName: TEdit;
    rb_ShowText: TRadioButton;
    mem_Text: TMemo;
    procedure NPipeClient1StartWork(Sender: TObject; bytesTotal: Int64;
      WorkMode: TNPWorkMode);
    procedure NPipeClient1EndWork(Sender: TObject);
    procedure NPipeClient1Progress(Sender: TObject; bytesCurrent: Int64);
    procedure btn_SendTextClick(Sender: TObject);
    procedure btn_SendFileClick(Sender: TObject);
    procedure NPipeClient1ServerReply(Sender: TObject; Reply: TMemoryStream);
    procedure NPipeClient1Error(Sender: TObject; AException: Exception);
    procedure tb_SendTimeoutChange(Sender: TObject);
    procedure ed_PipeserverChange(Sender: TObject);
    procedure ed_PipenameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
  ed_SaveFileName.Text:='c:\temp\server_reply.txt';
  ed_FileName.Text:=Application.ExeName;
  ed_Pipename.Text:=NPipeClient1.PipeName;
  ed_Pipeserver.Text:=NPipeClient1.PipeServer;
  if (NPipeClient1.SendTimeout<=tb_SendTimeout.Max) then
    tb_SendTimeout.Position:=NPipeClient1.SendTimeout;
end;

procedure TForm4.ed_PipenameChange(Sender: TObject);
begin
  NPipeClient1.PipeName:=ed_Pipename.Text;
end;

procedure TForm4.ed_PipeserverChange(Sender: TObject);
begin
  NPipeClient1.PipeServer:=ed_Pipeserver.Text;
end;

procedure TForm4.tb_SendTimeoutChange(Sender: TObject);
begin
  NPipeClient1.SendTimeout:=tb_SendTimeout.Position;
  lbl_SendTimeout.Caption:='Send timeout: '+IntToStr(tb_SendTimeout.Position);
end;

procedure TForm4.NPipeClient1Error(Sender: TObject; AException: Exception);
begin
  MessageDlg(AException.Message, mtError, [mbOK], 0);
end;

procedure TForm4.NPipeClient1ServerReply(Sender: TObject; Reply: TMemoryStream);
begin
  if (rb_ShowText.Checked) then
    mem_Text.Lines.LoadFromStream(Reply)
  else
  begin
    try
      Reply.SaveToFile(ed_SaveFileName.Text);
    except
      on e:Exception do
        MessageDlg(e.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TForm4.btn_SendFileClick(Sender: TObject);
var
  MS : TMemoryStream;
begin
  if (FileExists(ed_FileName.Text)) then
  begin
    Ms:=TMemoryStream.Create;
    Ms.LoadFromFile(ed_FileName.Text);
    NPipeClient1.SendData(MS);
    Ms.Free;
  end;
end;

procedure TForm4.btn_SendTextClick(Sender: TObject);
var
  MS : TMemoryStream;
begin
  MS:=TMemoryStream.Create;
  mem_Data.Lines.SaveToStream(MS);
  NPipeClient1.SendData(MS);
  MS.Free;
end;

procedure TForm4.NPipeClient1Progress(Sender: TObject; bytesCurrent: Int64);
begin
  pb.Position:=bytesCurrent;
end;

procedure TForm4.NPipeClient1EndWork(Sender: TObject);
begin
  pb.Position:=pb.Min;
end;

procedure TForm4.NPipeClient1StartWork(Sender: TObject; bytesTotal: Int64;
  WorkMode: TNPWorkMode);
begin
  pb.Max:=bytesTotal;
end;

end.
