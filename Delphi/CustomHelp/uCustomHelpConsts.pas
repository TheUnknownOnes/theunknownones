unit uCustomHelpConsts;

interface

type
  TNamespaceTrimOption = (nstoNoTrim=0,
                          nstoTrimFirst,
                          nstoTrimAll);

  TDisplayLocationOption = (dloWelcomePage=0,
                            dloDefaultBrowser,
                            dloMSDocumentExplorer);


const
  OPTIONS_NAMESPACETRIM : array [TNamespaceTrimOption] of string = ('no trim',
                                                    'trim first namespace',
                                                    'trim all namespaces');
  OPTIONS_DISPLAY_LOCATIONS : array [TDisplayLocationOption] of string =
                                  ('Delphi WelcomePage',
                                   'default browser',
                                   'Microsoft Document Explorer');

  REG_ROOT_BASE = '\TheUnknownOnes\Delphi';
  REG_ROOT_PROJECT = '\CustomHelp';

  {$ifDef VER170}
     REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER170' + REG_ROOT_PROJECT;
  {$Endif}
  {$ifDef VER180}
    {$ifDef VER185}
      REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER185' + REG_ROOT_PROJECT;
    {$Else}
      REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER180' + REG_ROOT_PROJECT;
    {$Endif}
  {$Endif}
  {$ifDef VER200}
    REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER200' + REG_ROOT_PROJECT;
  {$Endif}
  {$ifDef VER210}
    REG_ROOT_KEY = '\Software'+REG_ROOT_BASE+'\VER210' + REG_ROOT_PROJECT;
  {$Endif}

  PROTPREFIX_CUSTOMHELP = 'CustomHelp://';
  PROTPREFIX_MSHELP = 'ms-help://';
  PROTPREFIX_HTMLHELP = 'htmlhlp://';
  PROTPREFIX_WINHELP = 'winhlp://';
  PROTPREFIX_UNKNOWNHELP = 'unknown://';

  VALUE_NAME = 'Name';
  VALUE_DESCR = 'Description';
  VALUE_URL = 'URL';
  VALUE_TRIMNAMESPACE = 'TrimNamespaces';

  PROVIDER_SUB_KEY = '\Provider';
  SETTINGS_SUB_KEY= '\Settings';
  NAMESPACES_SUB_KEY = SETTINGS_SUB_KEY + '\NAMESPACES';
  EXPANDEDITEMS_SUB_KEY = SETTINGS_SUB_KEY + '\EXPANDED';
  RESULT_ORDER_SUB_KEY = SETTINGS_SUB_KEY + '\RESULTORDER';

  URL_SPLITTER = #1;

  SETTINGS_FULLTEXTSEARCH = 'FullTextSearch';
  SETTINGS_TRIMNAMESPACES = 'TrimNamespaces';
  SETTINGS_OHSATTOP = 'DisplayOHSAtTop';
  SETTING_WINHELPGIDCHECK = 'CheckWinHelpGID';
  SETTINGS_DISPLAY_LOCATION = 'DisplayLocation';

  GROUP_LABEL_WEB_BASED = 'Web based providers';
  GROUP_LABEL_FILE_BASED = 'File based providers';
  GROUP_LABEL_STANDARD = 'Other Help Providers';
  GROUP_LABEL_DUMMY_MSHELP2 = 'Microsoft Help 2.x';

  SETTINGS_COLOR_FILE_PROVIDER = 'Color File Provider';
  SETTINGS_COLOR_MSHELP = 'Color MSHelp 2';
  SETTINGS_COLOR_WEB_PROVIDER = 'Color Web Provider';

  ENVVAR_NAME_KEYWORD = 'HelpString';
  ENVVAR_NAME_KEYWORD_URL = 'HelpStringURLEncoded';
  KIBITZ_IGNORED_HELPSTRING = 'erroneous type';

implementation

end.
