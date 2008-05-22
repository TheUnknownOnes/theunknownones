unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettingsBase, uSettingsStream, StdCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Settings1: TSettingsFile;
    btn_ListUsersForm1Widths: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn_ListUsersForm1WidthsClick(Sender: TObject);
  private
    procedure ShowNameValueInList(const ANameValues : TSettingNameValues);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn_ListUsersForm1WidthsClick(Sender: TObject);
begin
  ShowNameValueInList(Settings1.GetNameValues('/Users/\w+/Form1/Width', true, true, true, true));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Settings1.SetValue('/Users/UserName1/Form1/Width', 100);
  Settings1.SetValue('/Users/UserName2/Form1/Width', 650);
  Settings1.SetValue('/Users/UserName3/Form1/Width', 63);
  Settings1.SetValue('/Users/UserName4/Form1/Width', 451);
  Settings1.SetValue('/Users/UserName5/Form1/Width', 10);
end;

procedure TForm1.ShowNameValueInList(const ANameValues: TSettingNameValues);
var
  idx : Integer;
begin
  ListBox1.Clear;

  for idx := Low(ANameValues) to High(ANameValues) do
    ListBox1.AddItem(ANameValues[idx].Name + ' = ' + IntToStr(ANameValues[idx].Value), nil);
end;

end.
