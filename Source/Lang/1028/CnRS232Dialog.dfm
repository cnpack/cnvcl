object CnRS232Dlg: TCnRS232Dlg
  Left = 238
  Top = 123
  BorderStyle = bsDialog
  Caption = '串口設置'
  ClientHeight = 316
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = CHINESEBIG5_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '細明體'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object pcCommConfig: TPageControl
    Left = 8
    Top = 8
    Width = 297
    Height = 273
    ActivePage = tsNormal
    TabOrder = 0
    object tsNormal: TTabSheet
      Caption = '常規設置'
      object lblBaudRate: TLabel
        Left = 16
        Top = 12
        Width = 60
        Height = 12
        Caption = '波特率(&B):'
      end
      object lblByteSize: TLabel
        Left = 16
        Top = 41
        Width = 60
        Height = 12
        Caption = '數據位(&D):'
      end
      object lblParity: TLabel
        Left = 16
        Top = 70
        Width = 72
        Height = 12
        Caption = '奇偶校驗(&P):'
      end
      object lblStopBits: TLabel
        Left = 16
        Top = 98
        Width = 60
        Height = 12
        Caption = '停止位(&S):'
      end
      object cbbBaudRate: TComboBox
        Left = 120
        Top = 8
        Width = 145
        Height = 20
        Hint = '串口通訊的最大速度'#13#10'單位：bps'
        ItemHeight = 12
        TabOrder = 0
        OnExit = cbbBaudRateExit
        Items.Strings = (
          '110'
          '300'
          '600'
          '1200'
          '2400'
          '4800'
          '9600'
          '14400'
          '19200'
          '38400'
          '56000'
          '57600'
          '115200'
          '128000'
          '256000')
      end
      object cbbByteSize: TComboBox
        Left = 120
        Top = 37
        Width = 145
        Height = 20
        Hint = '可用數據位數'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 1
        Items.Strings = (
          '5'
          '6'
          '7'
          '8')
      end
      object cbbParity: TComboBox
        Left = 120
        Top = 66
        Width = 145
        Height = 20
        Hint = '奇偶校驗方式'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 2
        OnChange = ControlChanged
        Items.Strings = (
          '無'
          '奇校驗'
          '偶校驗'
          '傳號校驗'
          '空號校驗 ')
      end
      object cbbStopBits: TComboBox
        Left = 120
        Top = 94
        Width = 145
        Height = 20
        Hint = '停止位數'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 3
        Items.Strings = (
          '1'
          '1.5'
          '2 ')
      end
      object cbReplaceWhenParityError: TCheckBox
        Left = 16
        Top = 128
        Width = 200
        Height = 17
        Hint = '出現奇偶校驗錯時用指定字符代替'
        Caption = '奇偶校驗錯誤替換成字符(&ASCII):'
        TabOrder = 4
        OnClick = ControlChanged
      end
      object cbIgnoreNullChar: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = '是否丟棄接收到的NULL(ASCII 0)字符'
        Caption = '忽略NULL字符(&N)'
        TabOrder = 6
      end
      object seReplacedChar: TCnSpinEdit
        Left = 216
        Top = 126
        Width = 49
        Height = 21
        Hint = '替換字符的ASCII碼'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
    end
    object tsXonXoff: TTabSheet
      Caption = '軟件流量控制'
      ImageIndex = 1
      object lblXonLimit: TLabel
        Left = 16
        Top = 87
        Width = 66
        Height = 12
        Caption = 'Xon閾值(&B):'
      end
      object lblXoffLimit: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 12
        Caption = 'Xoff閾值(&H):'
      end
      object lblXonChar: TLabel
        Left = 16
        Top = 146
        Width = 108
        Height = 12
        Caption = 'Xon字符(ASCII)(&K):'
      end
      object lblXoffChar: TLabel
        Left = 16
        Top = 176
        Width = 114
        Height = 12
        Caption = 'Xoff字符(ASCII)(&I):'
      end
      object cbTxContinueOnXoff: TCheckBox
        Left = 16
        Top = 56
        Width = 200
        Height = 17
        Hint = 
          '當接收緩衝區已滿，已發送「Xoff字符」後發送是否停止。'#13#10'如果選擇，' +
          '當被填滿的接收緩衝區中的字節數未達到「Xoff閾值」'#13#10'並且驅動程序發' +
          '送了「Xoff字符」後停止接收字節時，繼續發送；'#13#10'如果不選，當被排空' +
          '的緩衝區中的字節數不足「Xon閾值」個字'#13#10'節，且驅動程序發送了「Xon' +
          '字符」後恢復接收時，繼續發送。'
        Caption = 'Xoff後繼續發送數據(&C)'
        TabOrder = 2
      end
      object cbOutx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 32
        Width = 200
        Height = 17
        Hint = 
          '數據發送時是否使用Xon/Xoff信息流控制'#13#10'如果選擇，當接收到「Xoff字' +
          '符」時暫停發送，並在'#13#10'接收到「Xon字符」時恢復發送。'
        Caption = '輸出Xon/Xoff有效(&O)'
        TabOrder = 1
      end
      object cbInx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 8
        Width = 200
        Height = 17
        Hint = 
          '數據接收時是否使用Xon/Xoff信息流控制'#13#10'如果選擇，當接收緩衝區快滿' +
          '，只剩「Xoff閾值」個'#13#10'字符空閒時發送「Xoff字符」；當接收緩衝區中' +
          '只有'#13#10'「Xon閾值」個字符時，發送「Xon字符」。'
        Caption = '輸入Xon/Xoff有效(&F)'
        TabOrder = 0
      end
      object seXonLimit: TCnSpinEdit
        Left = 136
        Top = 83
        Width = 65
        Height = 21
        Hint = '指明在發送「Xon字符」之前，接收緩衝區中允許的最少字符數。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnExit = seXonLimitExit
      end
      object seXonChar: TCnSpinEdit
        Left = 136
        Top = 144
        Width = 65
        Height = 21
        Hint = '發送和接收的「Xon字符」的ASCII碼，表示允許繼續傳輸。'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
      object seXoffChar: TCnSpinEdit
        Left = 136
        Top = 174
        Width = 65
        Height = 21
        Hint = '發送和接收的「Xoff字符」的ASCII碼，表示允許暫停傳輸。'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnExit = seReplacedCharExit
      end
      object seXoffLimit: TCnSpinEdit
        Left = 136
        Top = 113
        Width = 65
        Height = 21
        Hint = 
          '指明在發送「Xoff字符」之前，接收緩衝區中允許的最多字符數。'#13#10'接收' +
          '緩衝區的長度減去該值，即允許的最多字符數。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnExit = seXonLimitExit
      end
    end
    object tsHardware: TTabSheet
      Caption = '硬件流量控制'
      ImageIndex = 2
      object lblDtrControl: TLabel
        Left = 16
        Top = 39
        Width = 90
        Height = 12
        Caption = 'DTR流量控制(&T):'
      end
      object lblRtsControl: TLabel
        Left = 16
        Top = 68
        Width = 90
        Height = 12
        Caption = 'RTS流量控制(&R):'
      end
      object lblInCtrl: TLabel
        Left = 16
        Top = 16
        Width = 54
        Height = 12
        Caption = '輸入控制:'
      end
      object lblOutCtrl: TLabel
        Left = 16
        Top = 108
        Width = 54
        Height = 12
        Caption = '輸出控制:'
      end
      object cbOutx_CtsFlow: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = 'CTS(清除發送)'
        Caption = '使用CTS信號進行輸出流量控制(&C)'
        TabOrder = 3
      end
      object cbOutx_DsrFlow: TCheckBox
        Left = 16
        Top = 128
        Width = 200
        Height = 17
        Hint = 'DSR(數據設備就緒)'
        Caption = '使用DSR信號進行輸出流量控制(&B)'
        TabOrder = 2
      end
      object cbDsrSensitivity: TCheckBox
        Left = 16
        Top = 176
        Width = 200
        Height = 17
        Hint = 
          '指定通信驅動程序對DSR信號的狀態是否敏感。'#13#10'如果選擇，當Modem的DS' +
          'R輸入線為低時，驅動程序將'#13#10'忽略接收到的任何字節。'
        Caption = 'DSR敏感度(&E)'
        TabOrder = 4
      end
      object cbbDtrControl: TComboBox
        Left = 120
        Top = 35
        Width = 145
        Height = 20
        Hint = '使用DTR(數據終端就緒)信號進行流量控制的方式'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 0
        Items.Strings = (
          '允許DTR線並保持'
          '禁止DTR線並保持'
          '允許DTR握手')
      end
      object cbbRtsControl: TComboBox
        Left = 120
        Top = 64
        Width = 145
        Height = 20
        Hint = '使用RTS(請求發送)信號進行流量控制的方式'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 1
        Items.Strings = (
          '允許RTS並保持'
          '禁止RTS並保持'
          '允許RTS握手'
          '使用觸發方式')
      end
    end
    object tsTimeouts: TTabSheet
      Caption = '超時設置'
      ImageIndex = 3
      object lblReadIntervalTimeout: TLabel
        Left = 16
        Top = 15
        Width = 84
        Height = 12
        Caption = '讀間隔超時(&R):'
      end
      object lblReadTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 45
        Width = 96
        Height = 12
        Caption = '讀總超時係數(&T):'
      end
      object lblMSec1: TLabel
        Left = 240
        Top = 16
        Width = 24
        Height = 12
        Caption = '毫秒'
      end
      object lblMSec2: TLabel
        Left = 240
        Top = 46
        Width = 24
        Height = 12
        Caption = '毫秒'
      end
      object lblReadTotalTimeoutConstant: TLabel
        Left = 16
        Top = 75
        Width = 96
        Height = 12
        Caption = '讀總超時常量(&A):'
      end
      object lblMSec3: TLabel
        Left = 240
        Top = 76
        Width = 24
        Height = 12
        Caption = '毫秒'
      end
      object lblWriteTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 105
        Width = 96
        Height = 12
        Caption = '寫總超時係數(&W):'
      end
      object lblMSec4: TLabel
        Left = 240
        Top = 106
        Width = 24
        Height = 12
        Caption = '毫秒'
      end
      object lblWriteTotalTimeoutConstant: TLabel
        Left = 16
        Top = 135
        Width = 96
        Height = 12
        Caption = '寫總超時常量(&B):'
      end
      object lblMSec5: TLabel
        Left = 240
        Top = 136
        Width = 24
        Height = 12
        Caption = '毫秒'
      end
      object seReadIntervalTimeout: TCnSpinEdit
        Left = 136
        Top = 11
        Width = 97
        Height = 21
        Hint = 
          '指定通信線路上兩個字符到達之間的最大時間。'#13#10'在讀取操作期間，從接' +
          '收到第一個字符時開始計時，'#13#10'若任意兩個字符到達之間的時間間隔超過' +
          '這個最大'#13#10'值，則讀取操作完成，返回緩衝數據。'#13#10'如果置0，表示不使' +
          '用間隔超時。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object seReadTotalTimeoutMultiplier: TCnSpinEdit
        Left = 136
        Top = 41
        Width = 97
        Height = 21
        Hint = 
          '用於設定讀總超時時間。'#13#10'讀總超時時間 = (總超時係數 X 接收字符數)' +
          ' + 總超時常量'#13#10'常量和係數可分別為0。'#13#10'如果均為0，則不使用總超時' +
          '設定。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object seReadTotalTimeoutConstant: TCnSpinEdit
        Left = 136
        Top = 71
        Width = 97
        Height = 21
        Hint = 
          '用於設定讀總超時時間。'#13#10'讀總超時時間 = (總超時係數 X 接收字符數)' +
          ' + 總超時常量'#13#10'常量和係數可分別為0。'#13#10'如果均為0，則不使用總超時' +
          '設定。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object seWriteTotalTimeoutMultiplier: TCnSpinEdit
        Left = 136
        Top = 101
        Width = 97
        Height = 21
        Hint = 
          '用於設定寫總超時時間。'#13#10'寫總超時時間 = (總超時係數 X 接收字符數)' +
          ' + 總超時常量'#13#10'常量和係數可分別為0。'#13#10'如果均為0，則不使用總超時' +
          '設定。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object seWriteTotalTimeoutConstant: TCnSpinEdit
        Left = 136
        Top = 131
        Width = 97
        Height = 21
        Hint = 
          '用於設定寫總超時時間。'#13#10'寫總超時時間 = (總超時係數 X 接收字符數)' +
          ' + 總超時常量'#13#10'常量和係數可分別為0。'#13#10'如果均為0，則不使用總超時' +
          '設定。'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 4
        Value = 0
      end
    end
  end
  object bbtnOk: TBitBtn
    Left = 144
    Top = 288
    Width = 75
    Height = 21
    Caption = '確定(&O)'
    Default = True
    TabOrder = 2
    OnClick = bbtnOkClick
  end
  object bbtnCancel: TBitBtn
    Left = 224
    Top = 288
    Width = 75
    Height = 21
    Cancel = True
    Caption = '取消(&C)'
    ModalResult = 2
    TabOrder = 3
  end
  object cbShowHint: TCheckBox
    Left = 8
    Top = 290
    Width = 105
    Height = 17
    Caption = '顯示提示信息'
    TabOrder = 1
    OnClick = cbShowHintClick
  end
end
