unit Unit42;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frxClass, frxDesgn, frxRich, uRichViewEditor, RVScroll,
  RichView, Menus;

type
  TForm42 = class(TForm)
    frxReport1: TfrxReport;
    Button1: TButton;
    frxDesigner1: TfrxDesigner;
    frxRichObject1: TfrxRichObject;
    RichViewEditor1: TRichViewEditor;
    MainMenu1: TMainMenu;
    abc1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form42: TForm42;

implementation

uses ufrxRichViewView;

{$R *.dfm}

procedure TForm42.Button1Click(Sender: TObject);
begin
  RichViewEditor1.SaveRVFToFile('c:\temp\test.rvf', False);

  if FileExists('C:\Temp\Test.fr3') then
    frxReport1.LoadFromFile('C:\Temp\Test.fr3');
  frxReport1.DesignReport;
end;

procedure TForm42.FormShow(Sender: TObject);
begin

  RichViewEditor1.LoadRVFFromFile('c:\temp\test.rvf');
end;

end.
