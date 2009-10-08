unit Unit_FormSmileySelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HTMLLite, XPMan, ExtCtrls, StdCtrls;

type
  TForm_SmileySelect = class(TForm)
    Smiler: ThtmlLite;
    PanelPanel1: TPanel;
    btnCancel: TButton;
    procedure SmilerHotSpotClick(Sender: TObject; const SRC: string;
      var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure SmilerImageRequest(Sender: TObject; const SRC: string;
      var Stream: TMemoryStream);
  private
    { Private-Deklarationen }
  public
    selectedSmiley : String;

    function SelectSmiley(aLeft, aTop: Integer): String;
  end;

implementation

uses unit_FormSB;

{$R *.dfm}

procedure TForm_SmileySelect.FormShow(Sender: TObject);
var
  i        : Integer;

  html     : TStrings;
  fieldCnt : Integer;
  RowColor : String;
  sl       : TStrings;
begin
  self.MakeFullyVisible;

  html:=TStringList.Create;
  sl:=TStringList.Create;

  html.Add('<html>');
  html.Add('<body margin="0">');
  html.Add('<table width="100%">');


  fieldCnt:=0;

  for i := Low(form_SB.smilies) to High(form_SB.smilies) do
  begin
    if (sl.IndexOf(form_SB.Smilies[i].Imag)<0) then
    begin
      sl.Add(form_SB.Smilies[i].Imag);
      
      inc(fieldCnt);

      if (fieldCnt mod 2) = 0 then
        RowColor:='#B8D8FF'
      else
        RowColor:='#B0D0FF';

      html.Add('<tr>');
      html.Add('<td bgcolor="'+RowColor+'" >');
      html.Add('<a href="'+form_SB.Smilies[i].Imag+'" />'+
               '<img border="0" src="'+form_SB.Smilies[i].Imag+'" />'+
               '</a>');
      html.Add('</td>');
      html.Add('<td bgcolor="'+RowColor+'" >');
      html.Add('<a href="'+form_SB.Smilies[i].Imag+'" />');
      html.Add('<font face="Tahoma" size="8px">');
      html.Add(form_SB.Smilies[i].Code);
      html.Add('</font>');
      html.Add('</a>');
      html.Add('</td>');
      html.Add('<td bgcolor="'+RowColor+'" >');
      html.Add('<a href="'+form_SB.Smilies[i].Imag+'" />');
      html.Add('<font face="Tahoma" size="8px">');
      html.Add('<b>'+form_SB.Smilies[i].Hint+'</b>');
      html.Add('</font>');
      html.Add('</a>');
      html.Add('</td>');
      html.Add('</tr>');
    end;
  end;

  //Abschluﬂ
  html.Add('</table>');
  html.Add('</body>');
  html.Add('</html>');

  Smiler.LoadStrings(html, 'smiler');

  sl.Free;
  html.Free;
end;

function TForm_SmileySelect.SelectSmiley(aLeft, aTop: Integer): String;
begin
  self.Left:=aLeft;
  Self.Top:=aTop;
  if self.ShowModal = mrOK then
    Result:=' '+selectedSmiley+' '
  else
    Result:='';
end;

procedure TForm_SmileySelect.SmilerHotSpotClick(Sender: TObject;
  const SRC: string; var Handled: Boolean);
var
  i : integer;
begin
  for I := Low(form_SB.smilies) to High(form_SB.smilies) do
    if (form_SB.smilies[i].Imag=SRC) then
    begin
      selectedSmiley:=form_SB.smilies[i].Code;
      ModalResult:=mrOK;
      break;
    end;
  Handled:=true;
end;

procedure TForm_SmileySelect.SmilerImageRequest(Sender: TObject; const SRC: string;
  var Stream: TMemoryStream);
begin
  form_SB.SBImageRequest(Sender, SRC, Stream);
end;

end.
