object Form_PNGObjectList: TForm_PNGObjectList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit PNGObjectList'
  ClientHeight = 315
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 174
    Height = 165
    Align = alLeft
    OnPaint = PaintBoxPaint
    ExplicitHeight = 178
  end
  object pan_Right: TPanel
    Left = 529
    Top = 0
    Width = 99
    Height = 171
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 93
      Height = 25
      Align = alTop
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 93
      Height = 25
      Align = alTop
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btn_Apply: TButton
      AlignWithMargins = True
      Left = 3
      Top = 65
      Width = 93
      Height = 25
      Align = alTop
      Caption = 'Apply'
      Enabled = False
      TabOrder = 2
      OnClick = btn_ApplyClick
    end
  end
  object pan_List: TPanel
    Left = 0
    Top = 171
    Width = 628
    Height = 144
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object lv_Images: TListView
      AlignWithMargins = True
      Left = 127
      Top = 3
      Width = 498
      Height = 107
      Align = alClient
      Columns = <>
      HideSelection = False
      LargeImages = iml_LV
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnSelectItem = lv_ImagesSelectItem
    end
    object pan_Bottom: TPanel
      Left = 0
      Top = 113
      Width = 628
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btn_Add: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 100
        Height = 25
        Align = alLeft
        Caption = 'Add image'
        TabOrder = 0
        OnClick = btn_AddClick
      end
      object btn_Replace: TButton
        AlignWithMargins = True
        Left = 109
        Top = 3
        Width = 100
        Height = 25
        Align = alLeft
        Caption = 'Replace image'
        TabOrder = 1
        OnClick = btn_ReplaceClick
      end
      object btn_Delete: TButton
        AlignWithMargins = True
        Left = 215
        Top = 3
        Width = 100
        Height = 25
        Align = alLeft
        Caption = 'Delete image'
        TabOrder = 2
        OnClick = btn_DeleteClick
      end
      object btn_DeleteAll: TButton
        AlignWithMargins = True
        Left = 321
        Top = 3
        Width = 100
        Height = 25
        Align = alLeft
        Caption = 'Delete all'
        TabOrder = 3
        OnClick = btn_DeleteAllClick
      end
      object btn_MoveBack: TButton
        AlignWithMargins = True
        Left = 427
        Top = 3
        Width = 86
        Height = 25
        Align = alLeft
        Caption = 'Move back'
        Enabled = False
        TabOrder = 4
        OnClick = btn_MoveBackClick
      end
      object btn_MoveForw: TButton
        AlignWithMargins = True
        Left = 519
        Top = 3
        Width = 85
        Height = 25
        Align = alLeft
        Caption = 'Move forward'
        Enabled = False
        TabOrder = 5
        OnClick = btn_MoveForwClick
      end
    end
    object rg_ImgSize: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 118
      Height = 107
      Align = alLeft
      Caption = 'Preview size'
      TabOrder = 2
      OnClick = rg_ImgSizeClick
    end
  end
  object iml_LV: TImageList
    Height = 48
    Width = 48
    Left = 352
    Top = 212
  end
  object dlg_OpenImage: TOpenPictureDialog
    Filter = 'Portable Network Graphic (.png)|*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Find image ...'
    Left = 400
    Top = 216
  end
end
