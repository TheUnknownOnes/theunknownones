unit unitResourceDotNet;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceElement;

type

  TDotNetResourceElement = class (TResourceElement)
    private
      FType     : String;
      FMimeType : String;
      FComment  : String;
      function GetText: string;
      procedure SetText(const Value: string);
    protected
    public
      property Text : string read GetText write SetText;
      property rType: string read FType write FType;
      property MimeType: String read FMimeType write FMimeType;
      property Comment: String read FComment write FComment;
      class function GetBaseType : string; override;
    end;

implementation

{ TDotNetResourceElement }

class function TDotNetResourceElement.GetBaseType: string;
begin
  result := 'DotNet';
end;

function TDotNetResourceElement.GetText: string;
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

procedure TDotNetResourceElement.SetText(const Value: string);
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
