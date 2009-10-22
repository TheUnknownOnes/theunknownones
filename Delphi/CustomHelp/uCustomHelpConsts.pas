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

  PROTPREFIX_CUSTOMHELP = 'CustomHelp://';
  PROTPREFIX_MSHELP = 'ms-help://';
  PROTPREFIX_HTMLHELP = 'htmlhlp://';
  PROTPREFIX_WINHELP = 'winhlp://';
  PROTPREFIX_UNKNOWNHELP = 'unknown://';

  VALUE_NAME = 'Name';
  VALUE_DESCR = 'Description';
  VALUE_URL = 'URL';
  VALUE_TRIMNAMESPACE = 'TrimNamespaces';
  VALUE_ENABLED = 'Enabled';

  PROVIDER_SUB_KEY = '\Provider';
  RSS_PROVIDER_SUB_KEY = '\RSSProvider';
  SETTINGS_SUB_KEY= '\Settings';
  NAMESPACES_SUB_KEY = SETTINGS_SUB_KEY + '\NAMESPACES';
  EXPANDEDITEMS_SUB_KEY = SETTINGS_SUB_KEY + '\EXPANDED';
  RESULT_ORDER_SUB_KEY = SETTINGS_SUB_KEY + '\RESULTORDER';

  URL_SPLITTER = #$1D;
  URL_SEPERATOR = #$1E;

  SETTINGS_FULLTEXTSEARCH = 'FullTextSearch';
  SETTINGS_TRIMNAMESPACES = 'TrimNamespaces';
  SETTINGS_OHSATTOP = 'DisplayOHSAtTop';
  SETTING_WINHELPGIDCHECK = 'CheckWinHelpGID';
  SETTINGS_DISPLAY_LOCATION = 'DisplayLocation';

  GROUP_LABEL_WEB_BASED = 'Web based providers';
  GROUP_LABEL_FILE_BASED = 'File based providers';
  GROUP_LABEL_STANDARD = 'Other Help Providers';
  GROUP_LABEL_DUMMY_MSHELP2 = 'Microsoft Help 2.x';
  GROUP_PREFIX_RSS = 'RSS Result for ';

  SETTINGS_COLOR_FILE_PROVIDER = 'Color File Provider';
  SETTINGS_COLOR_MSHELP = 'Color MSHelp 2';
  SETTINGS_COLOR_WEB_PROVIDER = 'Color Web Provider';
  SETTINGS_COLOR_RSS_PROVIDER = 'Color RSS Provider';

  ENVVAR_NAME_KEYWORD = 'HelpString';
  ENVVAR_NAME_KEYWORD_URL = 'HelpStringURLEncoded';
  KIBITZ_IGNORED_HELPSTRING = 'erroneous type';

implementation

end.
