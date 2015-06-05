# Overview #
  * Help via F1 key
  * Search in all installed Hx namespaces (Microsoft Developer Help) (e.g. Jedi Help)
  * Search for help in the old (`*`.hlp) Windows help files (e.g. Delphi 7 help)
  * Search in Html-Help (`*`.chm) Files
  * Search using Index of fulltext
  * NEW in Version 1.3: search using the _Windows Search_
  * NEW in Version 2.0: native search in Google Codesearch
  * NEW in Version 2.0: Extend CustomHelp with your own provider or GUI
  * NEW in Version 2.0: Extended formating of search results
  * NEW in Version 2.0: much more ... just try it
  * **NEW in Version 2.1:** improved performance

# How to get it and installation #
  * Download the latest stable release at: http://theunknownones.googlecode.com/svn/tags/Release/CustomHelp/
or
  * Get the development package at: http://theunknownones.googlecode.com/svn/trunk/Delphi/CustomHelp/
or
  * Checkout the whole repository at: http://theunknownones.googlecode.com/svn/trunk/ using a SVN client ...
  * Simply load the package and install it

# How to configure #
  * The configuration can be found in the main menu at Help->Configure Custom Help...

  * Help Display
> > use WelcomePage to display help ... if unchecked the help will be displayed in the system's default browser

  * Help Namespaces
> > you may check as many hx help namespaces as you like. For the best results you should choose the help version appropriate your Delphi version. (borland.bds4 for Delphi2006 or ebarcadero.rs2009 for Delphi 2009). If JCL-help or JVCL-help is installed on your system you may select them too


> Perform fulltext search ... if checked all namespaces will be searched using full text analysis. You will get more results and the search lasts longer. Not all too useful ;-)

  * Other Help Sources
> > You may add URLs of your favourite online help provider (e.g. google codesearch) be aware that the keyword is appended to the URL. You may as well add a path to a `*`.hlp or `*`.chm file. Those help sources will be searched using the index. If the help file does not support index queries the help will not work. You may also choose to execute a command of your choice (e.g. `"C:\Program Files (x86)\Adobe\Reader 9.0\Reader\AcroRd32.exe" /A "zoom=50&navpanes=1=OpenActions&search=$(HelpString)" "C:\Users\JohnDoe\Documents\Help\BDS4 Reference.pdf"`. This will open a PDF reader and search for the HelpString. You may use any environment variable enclosed in "$()". `$(HelpString)` will always be replaced by the keyword to search for.

  * Windows Search
> > You can add different queries that are passed to the Windows Search to search in the system index. You can for example use this query to search whether your keyword was implemented in the jwapi:
```
ext:pas path:"C:\Users\YourUserName\Documents\Projects\Jedi\jwapi" $(HelpString)  
```
> > Note that you have to use the clear text filter for Pascal files.
> > A full reference of the available search query parameters can be found [here](http://www.microsoft.com/windows/products/winfamily/desktopsearch/technicalresources/advquery.mspx)

A full description can be found here-> http://www.delphipraxis.net/topic165769_ideexperte+customhelp.html&highlight=
it's German ... sorry ;-)
A short demo video is available here -> http://www.vimeo.com/15021497

Have fun with it
by TUO