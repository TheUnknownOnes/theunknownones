object FBPScript: TFBPScript
  OldCreateOrder = False
  Height = 349
  Width = 464
  object DefaultDatabase: TIBDatabase
    LoginPrompt = False
    DefaultTransaction = DefaultTransaction
    Left = 16
    Top = 8
  end
  object DefaultTransaction: TIBTransaction
    DefaultDatabase = DefaultDatabase
    Left = 16
    Top = 40
  end
  object DefaultQuery: TIBQuery
    Database = DefaultDatabase
    Transaction = DefaultTransaction
    Left = 16
    Top = 72
  end
end
