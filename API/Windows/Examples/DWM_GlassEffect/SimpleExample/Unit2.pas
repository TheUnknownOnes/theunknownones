unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, gdipobj;

type
  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.CreateParams(var Params: TCreateParams);
begin
  Params.ExStyle:=Params.ExStyle or WS_EX_LAYERED;
  inherited;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  DoubleBuffered:=true;
end;

procedure TForm2.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var
  brush : TBrush;
begin
  inherited;
  
  brush:=TBrush.Create;
  brush.Color:=Self.Color;

  FillRect(Message.DC,Self.ClientRect, brush.Handle);
  brush.free;

  Message.Result:=1;
end;


end.
