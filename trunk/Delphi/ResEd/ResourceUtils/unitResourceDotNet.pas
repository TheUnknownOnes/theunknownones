unit unitResourceDotNet;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceElement;

type

  TDotNetResourceElement = class (TResourceElement)
    private
      FType     : AnsiString;
      FMimeType : AnsiString;
      FComment  : AnsiString;
      function GetText: AnsiString;
      procedure SetText(const Value: AnsiString);
    protected
    public
      property Text : AnsiString read GetText write SetText;
      property rType: AnsiString read FType write FType;
      property MimeType: AnsiString read FMimeType write FMimeType;
      property Comment: AnsiString read FComment write FComment;
      class function GetBaseType : AnsiString; override;
    end;

implementation

{ TDotNetResourceElement }

class function TDotNetResourceElement.GetBaseType: AnsiString;
begin
  result := 'DotNet';
end;

function TDotNetResourceElement.GetText: AnsiString;
var
  s : TStringStream;
begin
  s := TStringStream.Create ('');
  try
    data.Seek (0, soFromBeginning);
    data.SaveToStream(s);
    data.Seek (0, soFromBeginning);
    Result := s.DataString;
  finally
    s.Free
  end;
end;

procedure TDotNetResourceElement.SetText(const Value: AnsiString);
var
  s : TStringStream;
  m : TMemoryStream;
begin
  s:=TStringStream.Create(Value);
  try
    m:=TMemoryStream.Create;
    try
      s.Seek(0, soFromBeginning);
      m.LoadFromStream(s);
      ChangeData(m);
    finally
      m.Free;
    end;
  finally
    s.Free;
  end;
end;

initialization
  RegisterResourceElement (TDotNetResourceElement);
finalization
  UnregisterResourceElement (TDotNetResourceElement);

end.
