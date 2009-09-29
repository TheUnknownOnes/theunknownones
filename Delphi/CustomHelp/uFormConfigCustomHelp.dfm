object form_Config: Tform_Config
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Custom Help'
  ClientHeight = 436
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 401
    Width = 487
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 328
      Top = 3
      Width = 75
      Height = 26
      Align = alRight
      Caption = '&OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 409
      Top = 3
      Width = 75
      Height = 26
      Align = alRight
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ListView1: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 487
    Height = 320
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Description'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'URL (Keyword will be appended)'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
  end
  object Panel2: TPanel
    Left = 0
    Top = 326
    Width = 493
    Height = 72
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 6
      Top = 6
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object Label2: TLabel
      Left = 6
      Top = 29
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object Label3: TLabel
      Left = 6
      Top = 52
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object edName: TEdit
      Left = 65
      Top = 3
      Width = 422
      Height = 21
      TabOrder = 0
      OnChange = edNameChange
    end
    object edDesc: TEdit
      Left = 65
      Top = 26
      Width = 422
      Height = 21
      TabOrder = 1
      OnChange = edDescChange
    end
    object edURL: TEdit
      Left = 65
      Top = 48
      Width = 422
      Height = 21
      TabOrder = 2
      OnChange = edURLChange
    end
  end
end
