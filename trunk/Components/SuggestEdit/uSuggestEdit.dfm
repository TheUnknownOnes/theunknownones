object FormEditSuggest: TFormEditSuggest
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FormEditSuggest'
  ClientHeight = 129
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 12
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 257
    Height = 114
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'meinten Sie'
      end
      item
        Caption = #196'hnlichkeit'
        Width = 60
      end>
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnCompare = ListView1Compare
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
    ExplicitHeight = 112
  end
  object Panel1: TPanel
    Left = 0
    Top = 114
    Width = 257
    Height = 15
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      Left = 240
      Top = 0
      Width = 17
      Height = 15
      Align = alRight
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton1Click
      ExplicitLeft = 234
      ExplicitHeight = 17
    end
  end
end
