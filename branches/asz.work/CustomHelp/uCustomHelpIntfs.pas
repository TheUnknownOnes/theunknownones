unit uCustomHelpIntfs;

interface

uses
  Classes, HelpIntfs;

type
  { ICustomHelpProvider.
    TranslateHelpString() is used to get information about a HelpString
    provided by GetHelpStrings().
    The class is asked to supply a list of custom help uri's through
    GetHelpStrings(); CustomHelp allows the user to choose one, and then calls
    ShowCustomHelp(), if the option 'replace default viewer' is not checked.
    Otherwise, the link given in the selected uri will be used to show help
    with the default viewing system.
     }
  ICustomHelpProvider = interface(ICustomHelpViewer)
    ['{DFCBE0F2-B3AC-4D1D-B3FF-919FEAD3988B}']
    function TranslateHelpString(const HelpString: string; var Caption, Description, Link, Group: string): Boolean;
    function GetCustomHelpStrings(const HelpString: string) : TStringList;
    procedure ShowCustomHelp(const HelpString: string);
  end;

implementation

end.
