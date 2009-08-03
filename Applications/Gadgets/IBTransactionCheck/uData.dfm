object Data: TData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 347
  Width = 476
  object DB: TpFIBDatabase
    DBParams.Strings = (
      'lc_ctype=ISO8859_1')
    SQLDialect = 3
    Timeout = 0
    WaitForRestoreConnect = 0
    Left = 224
    Top = 113
  end
  object Transe: TpFIBTransaction
    DefaultDatabase = DB
    TimeoutAction = TARollback
    CSMonitorSupport.Enabled = csmeDatabaseDriven
    Left = 224
    Top = 160
  end
  object qry_1: TpFIBQuery
    Transaction = Transe
    Database = DB
    SQL.Strings = (
      'select a.tmp$user Username,'
      
        '       f_lrtrim(a.tmp$user_ip_addr) || '#39' ('#39' || f_lrtrim(a.tmp$us' +
        'er_host) || '#39')'#39' Hostname,'
      '       f_minutesbetween(max(t.tmp$timestamp), '#39'now'#39') Len,'
      '       t.tmp$attachment_id Attachment'
      'from tmp$attachments a'
      '  inner join tmp$transactions t'
      '    on t.tmp$attachment_id = a.tmp$attachment_id'
      'where t.tmp$state = '#39'ACTIVE'#39
      
        'group by a.tmp$user, a.tmp$user_ip_addr, a.tmp$user_host, t.tmp$' +
        'attachment_id'
      'having f_minutesbetween(max(t.tmp$timestamp), '#39'now'#39') > 0'
      'order by 3 desc'
      '')
    CSMonitorSupport.Enabled = csmeTransactionDriven
    Left = 224
    Top = 224
  end
  object SettingsFile: TSettingsFile
    Left = 72
    Top = 40
  end
end
