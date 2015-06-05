This Component is part of the [TaskbarListComponents](TaskbarListComponents.md) package

### Class members ###

  * **procedure Initialize**
> > manually initialize the component if AutoInitialize is false

  * **property AutoInitialize : Boolean**
> > component is initialized on construction

  * **property ThumbBarButtons: [TThumbBarButtons](TThumbBarButtons.md)**
> > List of [TTaskbarListThumbButton](TTaskbarListThumbButton.md) elements that can be displayed in the taskbarlist item of your application

  * **property ThumbBarImages: TCustomImageList**
> > Imagelist for the glyps that shall be displayed in the buttons.


### How to use it ###

Simply drop an TTaskBarListThumbButtons component on your form and add some ThumbBarButtons. Turn AutoInitialize True and start your app.