### What are TaskbarListComponents? ###

TaskbarListComponents are a set of components designed as Delphi wrappers for the Windows 7 Taskbarlist Interfaces (e.g. [ITaskbarlist3](http://msdn.microsoft.com/en-us/library/dd391692%28VS.85%29.aspx))


### What's in the box? ###

Currently TUO's TaskbarList components support the following features:
  * **[TTaskbarListProgress](TTaskbarListProgress.md)** - wrapper for connecting Delphi progress bars to the taskbar item of the application. You can also manually set the progress values.
  * **[TTaskBarListOverlayIcon](TTaskBarListOverlayIcon.md)** - easily add an overlay icon to the taskbar item of your application
  * **[TTaskBarListThumbButtons](TTaskBarListThumbButtons.md)** - manage your application's thumb buttons in the taskbar
  * **[TTaskbarListFormTab](TTaskbarListFormTab.md)** - display any of your forms as separate tabs in the taskbar
  * **[TTaskbarListControlTab](TTaskbarListControlTab.md)** - display any control as separate tab in the taskbar

### I would like to see it first ###

  * http://www.vimeo.com/14291783 TTaskbarListThumbButtons and TTaskbarListOverlayIcon
  * http://www.vimeo.com/14354328 TTaskbarListProgress
  * http://www.vimeo.com/14356627 TTaskbarlistFormTab and TTaskbarlistControlTab



### How to install? ###

Checkout or download the source at:
http://code.google.com/p/theunknownones/source/browse/trunk#trunk/Components/TaskBarList

The folder contains the packages for Delphi 2006 and 2009 already. Simply make a package for your own Delphi version if it is not already there.

### Any prerequisites? ###

The package has to be compiled against the trunk revision of JEDI Windows API Library found here:
http://jedi-apilib.svn.sourceforge.net/viewvc/jedi-apilib/jwapi/trunk/
or checkout here
https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwapi/trunk/