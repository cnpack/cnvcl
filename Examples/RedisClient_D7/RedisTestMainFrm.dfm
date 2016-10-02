object RedisTestFrm: TRedisTestFrm
  Left = 151
  Top = 72
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Redis Client Library Test - D7 and Above'
  ClientHeight = 541
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 358
    Width = 693
    Height = 6
    Cursor = crVSplit
    Align = alBottom
  end
  object mmoResult: TMemo
    Left = 0
    Top = 364
    Width = 693
    Height = 177
    Align = alBottom
    PopupMenu = PopupMenu1
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 693
    Height = 83
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblHost: TLabel
      Left = 60
      Top = 57
      Width = 336
      Height = 13
      Caption = 'Host:                    Port:         Password:'
    end
    object edtHost: TEdit
      Left = 97
      Top = 53
      Width = 132
      Height = 21
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 274
      Top = 53
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '8888'
    end
    object edtPassword: TEdit
      Left = 402
      Top = 53
      Width = 129
      Height = 21
      TabOrder = 2
      Text = 'mosker123'
    end
    object btnDisconnect: TButton
      Left = 538
      Top = 52
      Width = 91
      Height = 23
      Caption = 'Disconnect'
      TabOrder = 3
      OnClick = btnDisconnectClick
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 693
      Height = 46
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = #23435#20307
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      object Label5: TLabel
        Left = 214
        Top = 9
        Width = 260
        Height = 24
        Caption = 'Redis Client Library'
      end
      object Label6: TLabel
        Left = 494
        Top = 21
        Width = 168
        Height = 12
        Caption = 'Copyright (2016) ReverseKing'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = #23435#20307
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 83
    Width = 693
    Height = 275
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'KEY'
      object Label7: TLabel
        Left = 14
        Top = 140
        Width = 49
        Height = 13
        Caption = 'Cursor:'
      end
      object Label8: TLabel
        Left = 127
        Top = 140
        Width = 42
        Height = 13
        Caption = 'Count:'
      end
      object Label9: TLabel
        Left = 11
        Top = 34
        Width = 28
        Height = 13
        Caption = 'Sec:'
      end
      object Label10: TLabel
        Left = 11
        Top = 86
        Width = 21
        Height = 13
        Caption = 'DB:'
      end
      object lblKeyParam: TLabel
        Left = 11
        Top = 111
        Width = 42
        Height = 13
        Caption = 'Param:'
      end
      object lblKey: TLabel
        Left = 11
        Top = 3
        Width = 28
        Height = 13
        Caption = 'Key:'
      end
      object lblMatch: TLabel
        Left = 14
        Top = 165
        Width = 42
        Height = 13
        Caption = 'Match:'
      end
      object lblNewKey: TLabel
        Left = 14
        Top = 192
        Width = 49
        Height = 13
        Caption = 'NewKey:'
      end
      object btnKeyScan: TButton
        Left = 148
        Top = 161
        Width = 77
        Height = 22
        Caption = 'SCAN'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnKeyScanClick
      end
      object btnKeyDel: TButton
        Left = 139
        Top = 3
        Width = 77
        Height = 22
        Hint = '  DEL key [key ...]   '
        Caption = 'DEL'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnKeyDelClick
      end
      object SpinEdit1: TSpinEdit
        Left = 69
        Top = 137
        Width = 49
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 20
      end
      object SpinEdit2: TSpinEdit
        Left = 172
        Top = 137
        Width = 46
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 10
      end
      object btnKeyDump: TButton
        Left = 222
        Top = 3
        Width = 77
        Height = 22
        Hint = '  DUMP key  '
        Caption = '*DUMP'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnKeyDumpClick
      end
      object btnKeyExists: TButton
        Left = 305
        Top = 3
        Width = 77
        Height = 22
        Hint = '  EXISTS key '
        Caption = 'EXISTS'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = btnKeyExistsClick
      end
      object Button8: TButton
        Left = 139
        Top = 31
        Width = 77
        Height = 22
        Caption = 'EXPIRE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 222
        Top = 31
        Width = 77
        Height = 22
        Caption = 'EXPIREAT'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 305
        Top = 31
        Width = 77
        Height = 22
        Caption = 'KEYS'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = Button10Click
      end
      object btnKeyMigrate: TButton
        Left = 305
        Top = 59
        Width = 77
        Height = 22
        Caption = '*MIGRATE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        OnClick = btnKeyMigrateClick
      end
      object btnKeyMove: TButton
        Left = 97
        Top = 83
        Width = 77
        Height = 22
        Caption = 'MOVE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        OnClick = btnKeyMoveClick
      end
      object btnKeyObjCount: TButton
        Left = 388
        Top = 3
        Width = 77
        Height = 22
        Hint = '  OBJECT REFCOUNT <key>  '
        Caption = 'OBJCOUNT'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        OnClick = btnKeyObjCountClick
      end
      object Button14: TButton
        Left = 388
        Top = 31
        Width = 77
        Height = 22
        Caption = 'PERSIST'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        OnClick = Button14Click
      end
      object btnKeyRename: TButton
        Left = 388
        Top = 59
        Width = 77
        Height = 22
        Caption = 'RENAME'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
      end
      object btnPExpire: TButton
        Left = 231
        Top = 189
        Width = 77
        Height = 22
        Caption = 'PEXPIRE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 14
      end
      object btnPExpireat: TButton
        Left = 231
        Top = 163
        Width = 77
        Height = 22
        Caption = 'PEXPIREAT'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 15
        OnClick = btnPExpireatClick
      end
      object btnKeyRenameNX: TButton
        Left = 148
        Top = 189
        Width = 77
        Height = 22
        Caption = 'RENAMENX'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 16
        OnClick = btnKeyRenameNXClick
      end
      object btnKeyRestore: TButton
        Left = 314
        Top = 163
        Width = 77
        Height = 22
        Caption = 'RESTORE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 17
        OnClick = btnKeyRestoreClick
      end
      object btnKeyPTTL: TButton
        Left = 347
        Top = 83
        Width = 77
        Height = 22
        Caption = 'PTTL'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 18
        OnClick = btnKeyPTTLClick
      end
      object btnRandomKey: TButton
        Left = 314
        Top = 111
        Width = 77
        Height = 22
        Caption = 'RANDOMKEY'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 19
        OnClick = btnRandomKeyClick
      end
      object btnKeySort: TButton
        Left = 231
        Top = 111
        Width = 77
        Height = 22
        Caption = 'SORT'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 20
        OnClick = btnKeySortClick
      end
      object btnKeyTTL: TButton
        Left = 264
        Top = 83
        Width = 77
        Height = 22
        Caption = 'TTL'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 21
        OnClick = btnKeyTTLClick
      end
      object btnKeyType: TButton
        Left = 180
        Top = 84
        Width = 77
        Height = 22
        Caption = 'TYPE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 22
        OnClick = btnKeyTypeClick
      end
      object Sec_SE: TSpinEdit
        Left = 45
        Top = 31
        Width = 89
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 23
        Value = 10000
      end
      object edtDestIP: TEdit
        Left = 11
        Top = 59
        Width = 98
        Height = 21
        TabOrder = 24
        Text = '127.0.0.1'
      end
      object edtDestPort: TEdit
        Left = 115
        Top = 59
        Width = 49
        Height = 21
        TabOrder = 25
        Text = '9999'
      end
      object chkKeyCopy: TCheckBox
        Left = 170
        Top = 61
        Width = 77
        Height = 17
        Caption = 'COPY'
        TabOrder = 26
      end
      object chkKeyReplace: TCheckBox
        Left = 222
        Top = 61
        Width = 77
        Height = 17
        Caption = 'REPLACE'
        TabOrder = 27
      end
      object SpinEdit3: TSpinEdit
        Left = 38
        Top = 83
        Width = 53
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 28
        Value = 0
      end
      object btnKeyObjCode: TButton
        Left = 471
        Top = 3
        Width = 77
        Height = 22
        Hint = '  OBJECT ENCODING <key> '
        Caption = 'OBJCODE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 29
        OnClick = btnKeyObjCodeClick
      end
      object btnObjTime: TButton
        Left = 554
        Top = 3
        Width = 77
        Height = 22
        Hint = ' OBJECT IDLETIME <key> '
        Caption = 'OBJTIME'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 30
        OnClick = btnObjTimeClick
      end
      object edtKeyParam: TEdit
        Left = 66
        Top = 111
        Width = 159
        Height = 21
        TabOrder = 31
      end
      object edtKeyText: TEdit
        Left = 45
        Top = 3
        Width = 88
        Height = 21
        TabOrder = 32
        Text = 'mosker'
      end
      object edtMatch: TEdit
        Left = 65
        Top = 162
        Width = 77
        Height = 21
        TabOrder = 33
      end
      object edtNewKey: TEdit
        Left = 65
        Top = 189
        Width = 77
        Height = 21
        TabOrder = 34
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'String'
      ImageIndex = 1
      object Label1: TLabel
        Left = 11
        Top = 8
        Width = 34
        Height = 13
        Caption = 'Key'#65306
      end
      object Label2: TLabel
        Left = 22
        Top = 56
        Width = 48
        Height = 13
        Caption = 'Value'#65306
      end
      object Label3: TLabel
        Left = 40
        Top = 85
        Width = 168
        Height = 13
        Caption = #26102#38271#65306'       '#31186'    '#21442#25968#65306
      end
      object Label12: TLabel
        Left = 28
        Top = 111
        Width = 161
        Height = 13
        Caption = 'Start:            Stop:'
      end
      object btnStringSet: TButton
        Left = 183
        Top = 3
        Width = 77
        Height = 22
        Caption = 'SET'
        TabOrder = 0
        OnClick = btnStringSetClick
      end
      object btnStringGet: TButton
        Left = 266
        Top = 3
        Width = 77
        Height = 22
        Caption = 'Get'
        TabOrder = 1
        OnClick = btnStringGetClick
      end
      object edtStringKey: TEdit
        Left = 51
        Top = 5
        Width = 126
        Height = 21
        TabOrder = 2
        Text = 'people'
      end
      object edtStringValue: TEdit
        Left = 76
        Top = 52
        Width = 236
        Height = 21
        TabOrder = 3
        Text = 'This Value 123!'
      end
      object Edit6: TEdit
        Left = 76
        Top = 81
        Width = 48
        Height = 21
        TabOrder = 4
        Text = '0'
      end
      object ComboBox1: TComboBox
        Left = 206
        Top = 81
        Width = 106
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 5
        Text = #22987#32456#35774#32622
        Items.Strings = (
          #22987#32456#35774#32622
          #23384#22312#35774#32622
          #19981#23384#22312#35774#32622)
      end
      object btnStringBitCount: TButton
        Left = 349
        Top = 3
        Width = 77
        Height = 22
        Caption = 'BITCOUNT'
        TabOrder = 6
        OnClick = btnStringBitCountClick
      end
      object btnStringBitop: TButton
        Left = 429
        Top = 3
        Width = 77
        Height = 22
        Caption = 'BITOP'
        TabOrder = 7
      end
      object btnStringDecr: TButton
        Left = 512
        Top = 3
        Width = 77
        Height = 22
        Caption = 'DECR'
        TabOrder = 8
      end
      object btnStringDecrBy: TButton
        Left = 595
        Top = 3
        Width = 77
        Height = 22
        Caption = 'DECRBY'
        TabOrder = 9
      end
      object btnStringHGetAll: TButton
        Left = 429
        Top = 31
        Width = 77
        Height = 22
        Caption = 'HGETALL'
        TabOrder = 10
        OnClick = btnStringHGetAllClick
      end
      object Button34: TButton
        Left = 349
        Top = 31
        Width = 77
        Height = 22
        Caption = 'GETBIT'
        TabOrder = 11
      end
      object SpinEdit4: TSpinEdit
        Left = 76
        Top = 108
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 12
        Value = 0
      end
      object SpinEdit5: TSpinEdit
        Left = 195
        Top = 108
        Width = 61
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 13
        Value = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'HASH'
      ImageIndex = 2
    end
    object TabSheet4: TTabSheet
      Caption = 'LIST'
      ImageIndex = 3
    end
    object TabSheet5: TTabSheet
      Caption = 'Set'
      ImageIndex = 4
      object lblAMount: TLabel
        Left = 4
        Top = 41
        Width = 49
        Height = 13
        Caption = 'AMount:'
      end
      object lblLength: TLabel
        Left = 159
        Top = 41
        Width = 49
        Height = 13
        Caption = 'Length:'
      end
      object lblSetKey: TLabel
        Left = 19
        Top = 14
        Width = 28
        Height = 13
        Caption = 'Key:'
      end
      object lblSetKey1: TLabel
        Left = 283
        Top = 14
        Width = 28
        Height = 13
        Caption = 'Key:'
      end
      object lblSetKey2: TLabel
        Left = 427
        Top = 13
        Width = 28
        Height = 13
        Caption = 'Key:'
      end
      object btnSetAdd: TButton
        Left = 196
        Top = 10
        Width = 58
        Height = 25
        Caption = 'SADD'
        TabOrder = 0
        OnClick = btnSetAddClick
      end
      object Memo2: TMemo
        Left = 16
        Top = 72
        Width = 242
        Height = 137
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object SpinEdit6: TSpinEdit
        Left = 52
        Top = 38
        Width = 101
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 1000000
      end
      object SpinEdit7: TSpinEdit
        Left = 212
        Top = 41
        Width = 57
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 5
      end
      object edtSetKey: TEdit
        Left = 53
        Top = 11
        Width = 137
        Height = 21
        TabOrder = 4
      end
      object edtSetKey1: TEdit
        Left = 317
        Top = 12
        Width = 92
        Height = 21
        TabOrder = 5
      end
      object edtSetKey2: TEdit
        Left = 461
        Top = 11
        Width = 92
        Height = 21
        TabOrder = 6
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'SortedSet'
      ImageIndex = 5
    end
    object TabSheet7: TTabSheet
      Caption = 'Pub/Sub'
      ImageIndex = 6
    end
    object TabSheet8: TTabSheet
      Caption = 'Transaction'
      ImageIndex = 7
    end
    object TabSheet9: TTabSheet
      Caption = 'Script'
      ImageIndex = 8
    end
    object TabSheet10: TTabSheet
      Caption = 'Connection'
      ImageIndex = 9
    end
    object TabSheet11: TTabSheet
      Caption = 'Server'
      ImageIndex = 10
    end
  end
  object PopupMenu1: TPopupMenu
    AutoHotkeys = maManual
    Left = 24
    Top = 384
    object N1: TMenuItem
      Caption = '  '#28165'  '#31354
      OnClick = N1Click
    end
  end
end
