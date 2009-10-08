library ResXEd;


{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.XML.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Data.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Windows.Forms.dll'}
{$R 'AddResForm.TAddResForm.resources' 'AddResForm.resx'}

uses
  SysUtils,
  Classes,
  System.Reflection,
  System.Runtime.InteropServices,
  UnitMain in 'UnitMain.pas',
  AddResForm in 'AddResForm.pas' {AddResForm.TAddResForm: System.Windows.Forms.Form};

[assembly: AssemblyTitle('ResXEd')]
[assembly: AssemblyDescription('ResEd Extenstion for DotNet ResX Files')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('www.theunknownones.net')]
[assembly: AssemblyProduct('ResEd 1.6')]
[assembly: AssemblyCopyright('2006 by TUO')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

//
// Die Versionsinformation einer Assemblierung enthält die folgenden vier Werte:
//
//      Hauptversion
//      Nebenversion
//      Build-Nummer
//      Revision
//
// Sie können alle vier Werte festlegen oder für Revision und Build-Nummer die
// Standardwerte mit '*' - wie nachfolgend gezeigt - verwenden:

[assembly: AssemblyVersion('1.1.*')]

//
// Zum Signieren einer Assemblierung müssen Sie einen Schlüssel angeben. Weitere Informationen
// über das Signieren von Assemblierungen finden Sie in der Microsoft .NET Framework-Dokumentation.
//
// Mit den folgenden Attributen steuern Sie, welcher Schlüssel für die Signatur verwendet wird.

// Hinweise:
//   (*) Wenn kein Schlüssel angegeben wird, ist die Assemblierung nicht signiert.
//   (*) KeyName verweist auf einen Schlüssel, der im Crypto Service Provider
//      (CSP) auf Ihrem Rechner installiert wurde. KeyFile verweist auf eine
//       Datei, die einen Schlüssel enthält.
//   (*) Wenn sowohl der KeyFile- als auch der KeyName-Wert angegeben ist, wird
//       die folgende Verarbeitung durchgeführt:
//       (1) Wenn KeyName in dem CSP gefunden wird, wird dieser Schlüssel verwendet.
//       (2) Wenn KeyName nicht, aber KeyFile vorhanden ist, wird der Schlüssel
//           in KeyFile im CSP installiert und verwendet.
//   (*) Ein KeyFile können Sie mit dem Utility sn.exe (Starker Name) erzeugen.
//       Der Speicherort von KeyFile sollte relativ zum Projektausgabeverzeichnis
//       angegeben werden. Wenn sich Ihr KeyFile im Projektverzeichnis befindet,
//       würden Sie das Attribut AssemblyKeyFile folgendermaßen festlegen:
//       [assembly: AssemblyKeyFile('mykey.snk')], vorausgesetzt, Ihr
//       Ausgabeverzeichnis ist das Projektverzeichnis (Vorgabe).
//   (*) Verzögerte Signatur ist eine erweiterte Option; nähere Informationen
//       dazu finden Sie in der Microsoft .NET Framework-Dokumentation.
//
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]

//
// Verwenden Sie die folgenden Attribute zur Steuerung der COM-Sichtbarkeit Ihrer Assemblierung.
// Standardmäßig ist die gesamte Assemblierung für COM sichtbar. Die Einstellung false für ComVisible
// ist die für Ihre Assemblierung empfohlene Vorgabe. Um dann eine Klasse und ein Interface für COM
// bereitzustellen, setzen Sie jeweils ComVisible auf true. Es wird auch empfohlen das Attribut
// Guid hinzuzufügen.
//

[assembly: ComVisible(False)]
//[assembly: Guid('')]
//[assembly: TypeLibVersion(1, 0)]


begin
end.
