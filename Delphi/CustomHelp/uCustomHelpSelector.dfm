object FormHelpSelector: TFormHelpSelector
  Left = 0
  Top = 0
  ActiveControl = ListBox1
  Caption = 'Select Help Topic...'
  ClientHeight = 292
  ClientWidth = 769
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 763
    Height = 253
    Align = alClient
    Columns = <
      item
        Caption = 'Title'
        Width = 250
      end
      item
        AutoSize = True
        Caption = 'Description'
      end>
    GridLines = True
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnCompare = ListBox1Compare
    OnDblClick = ListBox1DblClick
    OnKeyPress = ListBox1KeyPress
  end
  object Panel1: TPanel
    Left = 0
    Top = 259
    Width = 769
    Height = 33
    Align = alBottom
    Anchors = [akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      769
      33)
    object btnOk: TButton
      Left = 691
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
  end
end
