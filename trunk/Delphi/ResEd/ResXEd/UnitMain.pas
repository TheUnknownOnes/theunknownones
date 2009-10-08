//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
{$Unsafecode ON}
unit UnitMain;

interface

uses
  System.Runtime.InteropServices,
  System.Drawing,
  System.Collections,
  System.Resources, System.Windows.Forms;

procedure RemoveFromResource([MarshalAs(UnmanagedType.Bstr)] const ResourceName : string;
                             [MarshalAs(UnmanagedType.Bstr)] const ResXName : string) unsafe; export;

procedure AddFileToResource([MarshalAs(UnmanagedType.Bstr)] const FileName : string;
                            [MarshalAs(UnmanagedType.Bstr)] const FileType : string;
                            [MarshalAs(UnmanagedType.Bstr)] const ResourceName : string;
                            [MarshalAs(UnmanagedType.Bstr)] const ResXName : string) unsafe; export;
implementation

uses
  System.Text, System.IO, AddResForm;

procedure AddImageToResource(FileName, ResName, ResXName: String);
var
  img    : System.Drawing.Bitmap;

  reader : System.Resources.ResXResourceReader;
  Writer : System.Resources.ResXResourceWriter;

  enum   : IDictionaryEnumerator;
begin
  Reader:=ResXResourceReader.Create(ResXName);
  Writer:=System.Resources.ResXResourceWriter.Create(ResXName);
  enum:=reader.GetEnumerator;
  while enum.MoveNext do
  begin
    if ResName<>enum.Key.ToString then
      Writer.AddResource(enum.Key.ToString, enum.Value);
  end;

  img:=System.Drawing.Bitmap.Create(FileName);
  Writer.AddResource(ResName, img);
  writer.Close;
end;

procedure AddIconToResource(FileName, ResName, ResXName: String);
var
  img    : System.Drawing.Icon;

  reader : System.Resources.ResXResourceReader;
  Writer : System.Resources.ResXResourceWriter;

  enum   : IDictionaryEnumerator;
begin
  Reader:=ResXResourceReader.Create(ResXName);
  Writer:=System.Resources.ResXResourceWriter.Create(ResXName);
  enum:=reader.GetEnumerator;
  while enum.MoveNext do
  begin
    if ResName<>enum.Key.ToString then
      Writer.AddResource(enum.Key.ToString, enum.Value);
  end;

  img:=System.Drawing.Icon.Create(FileName);
  Writer.AddResource(ResName, img);
  writer.Close;
end;

procedure AddStreamToResource(FileName, ResName, ResXName: String);
var
  str    : FileStream;
  Buffer : array of Byte;

  reader : System.Resources.ResXResourceReader;
  Writer : System.Resources.ResXResourceWriter;

  enum   : IDictionaryEnumerator;
begin
  Reader:=ResXResourceReader.Create(ResXName);
  Writer:=System.Resources.ResXResourceWriter.Create(ResXName);
  enum:=reader.GetEnumerator;
  while enum.MoveNext do
  begin
    if ResName<>enum.Key.ToString then
      Writer.AddResource(enum.Key.ToString, enum.Value);
  end;

  str:=FileStream.Create(FileName, FileMode.Open, FileAccess.Read);
  SetLength(Buffer, Str.Length);
  Str.Read(Buffer, 0, Length(Buffer));

  Writer.AddResource(ResName, Buffer);
  writer.Close;
end;

procedure AddFileToResource([MarshalAs(UnmanagedType.Bstr)] const FileName : string;
                            [MarshalAs(UnmanagedType.Bstr)] const FileType : string;
                            [MarshalAs(UnmanagedType.Bstr)] const ResourceName : string;
                            [MarshalAs(UnmanagedType.Bstr)] const ResXName : string) unsafe; export;
var
  AddResForm: TAddResForm;
begin
  if FileType='BITMAP' then
    AddImageToResource(FileName, ResourceName, ResXName)
  else
  if FileType='ICON' then
    AddIconToResource(FileName, ResourceName, ResXName)
  else
  if FileType='FILE' then
    AddStreamToResource(FileName, ResourceName, ResXName)
  else
  if FileType='OTHER' then
  begin
    AddResForm:=TAddResForm.Create;
    AddResForm.ResXFile:=ResXName;
    AddResForm.ShowDialog;
  end;
end;

procedure RemoveFromResource([MarshalAs(UnmanagedType.Bstr)] const ResourceName : string;
                             [MarshalAs(UnmanagedType.Bstr)] const ResXName : string) unsafe; export;
var
  reader : System.Resources.ResXResourceReader;
  Writer : System.Resources.ResXResourceWriter;

  enum   : IDictionaryEnumerator;
begin
  Reader:=ResXResourceReader.Create(ResXName);
  Writer:=System.Resources.ResXResourceWriter.Create(ResXName);
  enum:=reader.GetEnumerator;
  while enum.MoveNext do
  begin
    if ResourceName<>enum.Key.ToString then
      Writer.AddResource(enum.Key.ToString, enum.Value);
  end;
  writer.Close;
end;

exports
  AddFileToResource, RemoveFromResource;

end.
