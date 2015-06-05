This Component is part of the [TaskbarListComponents](TaskbarListComponents.md) package

### Class members ###

  * **procedure UpdateTaskWindow**
> > Call this to invalidate the preview image of the application

  * **property Active: Boolean**
> > rather self explaining

  * **property OnCloseTab: TCloseTabEvent**
> > what happens when you click on the close mark in the flyout

  * **property OnActivateTab: TNotifyEvent**
> > what happens when you click the tab in the flout

  * **property OnGetPreviewRect: TOnGetPreviewRect**
> > measurement of the preview to be drawn. Use only if you want to draw your own preview.

  * **property OnDrawPreview: TOnDrawPreview**
> > this is called if you want to supply an owner drawn image of your control

  * **property AutoInitialize**
> > the component is automatically activated on construction

  * **property TabProperties: TTabProperties**
> > any of the following values:
      * tpUseAppThumbnailAlways
> > > > Always use the thumbnail provided by the main application frame window rather than a thumbnail provided by the individual tab window.
      * tpUseAppThumbnailWhenActive
> > > > When the application tab is active and a live representation of its window is available, use the main application frame window thumbnail. At other times, use the tab window thumbnail.
      * tpUseAppPeekAlways
> > > > Always use the peek image provided by the main applicatio frame window rather than a peek image provided by the individual tab window
      * tpUseAppPeekWhenActive
> > > > When the application tab is active and a live representation of its window is available, show the main application frame in the peek feature. At other times, use the tab window.

  * **property InsertBefore: TTaskbarListTab**

> > any TTaskbarListTab in your application that you want to be inserted before. Note that this tab should be existing before you activate the current tab.

### How to use it ###

Simply drop an TTaskbarListFormTab component on your form. Turn AutoInitialize True and start your Application. Your form now displays as seperate tab in the taskbarlist item's flyout