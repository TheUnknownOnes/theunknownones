object FormEditSuggest: TFormEditSuggest
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FormEditSuggest'
  ClientHeight = 160
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 343
    Height = 160
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'meinten Sie'
      end
      item
        Caption = #196'hnlichkeit'
        Width = 80
      end>
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnCompare = ListView1Compare
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
  end
end
