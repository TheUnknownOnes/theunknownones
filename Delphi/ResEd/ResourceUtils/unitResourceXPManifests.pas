//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unitResourceXPManifests;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceElement, AxCtrls;

type
TXPManifestResourceElement = class (TAnsiResourceElement)
public
  class function GetBaseType : AnsiString; override;
  procedure InitNew; override;
end;

const
  RT_XPMANIFEST = MakeIntResource (24);

implementation

{ TXPManifestResourceElement }

const

 {
'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13#10+
'<assembly xmlns="urn:schemas-microsoft-com:asm.v1"'#13#10+
'manifestVersion="1.0">'#13#10+
'<assemblyIdentity'#13#10+
'    name="Woozle.PEResourceExplorer.XPManifest"'#13#10+
'    processorArchitecture="x86"'#13#10+
'    version="1.0.0.0"'#13#10+
'    type="win32"/>'#13#10+
'<description>Windows Shell</description>'#13#10+
'<dependency>'#13#10+
'    <dependentAssembly>'#13#10+
'        <assemblyIdentity'#13#10+
'            type="win32"'#13#10+
'            name="Microsoft.Windows.Common-Controls"'#13#10+
'            version="6.0.0.0"'#13#10+
'            processorArchitecture="x86"'#13#10+
'            publicKeyToken="6595b64144ccf1df"'#13#10+
'            language="*"'#13#10+
'        />'#13#10+
'    </dependentAssembly>'#13#10+
'</dependency>'#13#10+
'</assembly>';   }
  manifest : AnsiString =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'+
    '<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">'+
    '  <assemblyIdentity version="1.0.0.0"'+
    '    processorArchitecture="*"'+
    '     name="Application Name"'+
    '     type="win32"/>'+
    '  <description>Default Application Description</description>' +
    ' <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">' +
    '    <security>' +
    '      <requestedPrivileges>' +
    '        <requestedExecutionLevel' +
    '          level="asInvoker"' +
    '          uiAccess="False"/>' +
    '        </requestedPrivileges>' +
    '       </security>' +
    '  </trustInfo>' +
    '  <dependency>' +
    '     <dependentAssembly>' +
    '         <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" language="*" processorArchitecture="*" publicKeyToken="6595b64144ccf1df"/>' +
    '     </dependentAssembly>' +
    '  </dependency>' +
    '</assembly>';

class function TXPManifestResourceElement.GetBaseType: AnsiString;
begin
  result := IntToStr (Integer (RT_XPMANIFEST));
end;

procedure TXPManifestResourceElement.InitNew;
begin
  Data.Clear;
  Data.Write(PAnsiChar (manifest)^, Length (manifest))
end;

initialization
  RegisterResourceElement (TXPManifestResourceElement);
finalization
  UnRegisterResourceElement (TXPManifestResourceElement);
end.
