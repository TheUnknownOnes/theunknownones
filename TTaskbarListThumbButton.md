### Class members ###

  * **property Hint: String**
> > Tooltip text that is displayed for the button

  * **property Flags: TThumbBarButtonFlags**
> > this can hold any combination of following flags:
    * tbfDisabled - button is not enabled
    * tbfDismissOnClick - taskbarlist flyout will disappear on click
    * tbfNoBackground - there is no frame around the button
    * tbfHidden - button is invisible

  * **property ImageIndex: TImageIndex**
> > index of the image in the connected image list.

  * **property Action: TAction**
> > connected Action. Flags, ImageIndex and Click event will be retrieved from this.

  * **property OnClick: TNotifyEvent**
> > what will happen on click?