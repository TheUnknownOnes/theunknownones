UNIT uCustomHelpIntfs;

INTERFACE

USES
  Classes,
  HelpIntfs;

TYPE
  { ICustomHelpProvider.
    TranslateHelpString() is used to get information about a HelpString
    provided by GetHelpStrings().
    The class is asked to supply a list of custom help uri's through
    GetHelpStrings(); CustomHelp allows the user to choose one, and then calls
    ShowCustomHelp(), if the option 'replace default viewer' is not checked.
    Otherwise, the link given in the selected uri will be used to show help
    with the default viewing system.
     }
  ICustomHelpProvider = INTERFACE(ICustomHelpViewer)
    ['{DFCBE0F2-B3AC-4D1D-B3FF-919FEAD3988B}']
    FUNCTION TranslateHelpString(CONST HelpString: string;
      VAR Caption, Description, Link, Group: string): boolean;
    FUNCTION GetCustomHelpStrings(CONST HelpString: string): TStringList;
    PROCEDURE ShowCustomHelp(CONST HelpString: string);
  END;

  ICustomHelpKeywordRecorder = INTERFACE(IInterface)
    ['{2D69C7B6-0681-43CF-B995-F1A91C41E1BD}']
    FUNCTION GetKeywordList: TStringList;
    PROCEDURE SetKeywordList(CONST Value: TStringList);
    FUNCTION GetHelpStringList: TStringList;
    PROCEDURE SetHelpStringList(CONST Value: TStringList);
    FUNCTION GetShowHelpStringList: TStringList;
    PROCEDURE SetShowHelpStringList(CONST Value: TStringList);
    FUNCTION GetEnabled: boolean;
    PROCEDURE SetEnabled(CONST Value: boolean);
    PROCEDURE AddKeyword(HelpString: string; AIgnoreDuplicate: boolean = False);
    PROCEDURE Reset;
  END;

IMPLEMENTATION

END.

