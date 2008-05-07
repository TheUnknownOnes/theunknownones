object Form_SmileySelect: TForm_SmileySelect
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Form_SmileySelect'
  ClientHeight = 311
  ClientWidth = 350
  Color = clBlack
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Smiler: ThtmlLite
    Left = 0
    Top = 0
    Width = 350
    Height = 278
    OnHotSpotClick = SmilerHotSpotClick
    OnImageRequest = SmilerImageRequest
    TabOrder = 0
    Align = alClient
    DefBackground = 16767160
    BorderStyle = htNone
    HistoryMaxCount = 0
    DefFontName = 'Times New Roman'
    DefPreFontName = 'Courier New'
    DefHotSpotColor = 10040064
    DefVisitedLinkColor = 10040064
    DefOverLinkColor = 10040064
    NoSelect = False
    ScrollBars = ssVertical
    CharSet = DEFAULT_CHARSET
    htOptions = []
  end
  object PanelPanel1: TPanel
    Left = 0
    Top = 278
    Width = 350
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    Color = 16767160
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      350
      33)
    object btnCancel: TButton
      Left = 269
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Abbrechen'
      ModalResult = 2
      TabOrder = 0
    end
  end
end
