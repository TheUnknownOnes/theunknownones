This Component is part of the [TaskbarListComponents](TaskbarListComponents.md) package

### Class members ###

  * **property Min : ULONGLONG**
> > minimum value of progress
  * **property Max : ULONGLONG**
> > maximum value of progress
  * **property Position : ULONGLONG**
> > current Position of progress
  * **property State : TTaskbarListProgressState**
> > current state of progress. This can be any of the following values:
      * psNoProgress - there simply is no progress
      * psNormal - normal "green" progress bar
      * psPaused - "yellow" progress bar
      * psError - "red" progress bar
  * **property Marquee : Boolean**
> > is progress marquee? If true the above values will be ignored.

  * **property ProgressBar : TProgressBar**
> > progressbar that is connected to the taskbarlist item. This value overrides all previously set values above. To change the appearance of the tasklist progressbar simply change any property of this connected progress bar.

### How to use it ###

Simply drop an TTaskbarListProgress component on your form and connect it to an already existing progressbar on your form. Now the tasklist progress will do all the things the connected progressbar does.