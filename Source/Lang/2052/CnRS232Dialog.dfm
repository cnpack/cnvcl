object CnRS232Dlg: TCnRS232Dlg
  Left = 238
  Top = 123
  BorderStyle = bsDialog
  Caption = '串口设置'
  ClientHeight = 316
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
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
      Caption = '常规设置'
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
        Caption = '数据位(&D):'
      end
      object lblParity: TLabel
        Left = 16
        Top = 70
        Width = 72
        Height = 12
        Caption = '奇偶校验(&P):'
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
        Hint = '串口通讯的最大速度'#13#10'单位：bps'
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
        Hint = '可用数据位数'
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
        Hint = '奇偶校验方式'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 2
        OnChange = ControlChanged
        Items.Strings = (
          '无'
          '奇校验'
          '偶校验'
          '传号校验'
          '空号校验 ')
      end
      object cbbStopBits: TComboBox
        Left = 120
        Top = 94
        Width = 145
        Height = 20
        Hint = '停止位数'
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
        Hint = '出现奇偶校验错时用指定字符代替'
        Caption = '奇偶校验错误替换成字符(&ASCII):'
        TabOrder = 4
        OnClick = ControlChanged
      end
      object cbIgnoreNullChar: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = '是否丢弃接收到的NULL(ASCII 0)字符'
        Caption = '忽略NULL字符(&N)'
        TabOrder = 6
      end
      object seReplacedChar: TCnSpinEdit
        Left = 216
        Top = 126
        Width = 49
        Height = 21
        Hint = '替换字符的ASCII码'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
    end
    object tsXonXoff: TTabSheet
      Caption = '软件流量控制'
      ImageIndex = 1
      object lblXonLimit: TLabel
        Left = 16
        Top = 87
        Width = 66
        Height = 12
        Caption = 'Xon阈值(&B):'
      end
      object lblXoffLimit: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 12
        Caption = 'Xoff阈值(&H):'
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
          '当接收缓冲区已满，已发送“Xoff字符”后发送是否停止。'#13#10'如果选择，' +
          '当被填满的接收缓冲区中的字节数未达到“Xoff阈值”'#13#10'并且驱动程序发' +
          '送了“Xoff字符”后停止接收字节时，继续发送；'#13#10'如果不选，当被排空' +
          '的缓冲区中的字节数不足“Xon阈值”个字'#13#10'节，且驱动程序发送了“Xon' +
          '字符”后恢复接收时，继续发送。'
        Caption = 'Xoff后继续发送数据(&C)'
        TabOrder = 2
      end
      object cbOutx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 32
        Width = 200
        Height = 17
        Hint = 
          '数据发送时是否使用Xon/Xoff信息流控制'#13#10'如果选择，当接收到“Xoff字' +
          '符”时暂停发送，并在'#13#10'接收到“Xon字符”时恢复发送。'
        Caption = '输出Xon/Xoff有效(&O)'
        TabOrder = 1
      end
      object cbInx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 8
        Width = 200
        Height = 17
        Hint = 
          '数据接收时是否使用Xon/Xoff信息流控制'#13#10'如果选择，当接收缓冲区快满' +
          '，只剩“Xoff阈值”个'#13#10'字符空闲时发送“Xoff字符”；当接收缓冲区中' +
          '只有'#13#10'“Xon阈值”个字符时，发送“Xon字符”。'
        Caption = '输入Xon/Xoff有效(&F)'
        TabOrder = 0
      end
      object seXonLimit: TCnSpinEdit
        Left = 136
        Top = 83
        Width = 65
        Height = 21
        Hint = '指明在发送“Xon字符”之前，接收缓冲区中允许的最少字符数。'
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
        Hint = '发送和接收的“Xon字符”的ASCII码，表示允许继续传输。'
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
        Hint = '发送和接收的“Xoff字符”的ASCII码，表示允许暂停传输。'
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
          '指明在发送“Xoff字符”之前，接收缓冲区中允许的最多字符数。'#13#10'接收' +
          '缓冲区的长度减去该值，即允许的最多字符数。'
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
        Caption = '输入控制:'
      end
      object lblOutCtrl: TLabel
        Left = 16
        Top = 108
        Width = 54
        Height = 12
        Caption = '输出控制:'
      end
      object cbOutx_CtsFlow: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = 'CTS(清除发送)'
        Caption = '使用CTS信号进行输出流量控制(&C)'
        TabOrder = 3
      end
      object cbOutx_DsrFlow: TCheckBox
        Left = 16
        Top = 128
        Width = 200
        Height = 17
        Hint = 'DSR(数据设备就绪)'
        Caption = '使用DSR信号进行输出流量控制(&B)'
        TabOrder = 2
      end
      object cbDsrSensitivity: TCheckBox
        Left = 16
        Top = 176
        Width = 200
        Height = 17
        Hint = 
          '指定通信驱动程序对DSR信号的状态是否敏感。'#13#10'如果选择，当Modem的DS' +
          'R输入线为低时，驱动程序将'#13#10'忽略接收到的任何字节。'
        Caption = 'DSR敏感度(&E)'
        TabOrder = 4
      end
      object cbbDtrControl: TComboBox
        Left = 120
        Top = 35
        Width = 145
        Height = 20
        Hint = '使用DTR(数据终端就绪)信号进行流量控制的方式'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 0
        Items.Strings = (
          '允许DTR线并保持'
          '禁止DTR线并保持'
          '允许DTR握手')
      end
      object cbbRtsControl: TComboBox
        Left = 120
        Top = 64
        Width = 145
        Height = 20
        Hint = '使用RTS(请求发送)信号进行流量控制的方式'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 1
        Items.Strings = (
          '允许RTS并保持'
          '禁止RTS并保持'
          '允许RTS握手'
          '使用触发方式')
      end
    end
    object tsTimeouts: TTabSheet
      Caption = '超时设置'
      ImageIndex = 3
      object lblReadIntervalTimeout: TLabel
        Left = 16
        Top = 15
        Width = 84
        Height = 12
        Caption = '读间隔超时(&R):'
      end
      object lblReadTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 45
        Width = 96
        Height = 12
        Caption = '读总超时系数(&T):'
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
        Caption = '读总超时常量(&A):'
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
        Caption = '写总超时系数(&W):'
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
        Caption = '写总超时常量(&B):'
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
          '指定通信线路上两个字符到达之间的最大时间。'#13#10'在读取操作期间，从接' +
          '收到第一个字符时开始计时，'#13#10'若任意两个字符到达之间的时间间隔超过' +
          '这个最大'#13#10'值，则读取操作完成，返回缓冲数据。'#13#10'如果置0，表示不使' +
          '用间隔超时。'
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
          '用于设定读总超时时间。'#13#10'读总超时时间 = (总超时系数 X 接收字符数)' +
          ' + 总超时常量'#13#10'常量和系数可分别为0。'#13#10'如果均为0，则不使用总超时' +
          '设定。'
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
          '用于设定读总超时时间。'#13#10'读总超时时间 = (总超时系数 X 接收字符数)' +
          ' + 总超时常量'#13#10'常量和系数可分别为0。'#13#10'如果均为0，则不使用总超时' +
          '设定。'
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
          '用于设定写总超时时间。'#13#10'写总超时时间 = (总超时系数 X 接收字符数)' +
          ' + 总超时常量'#13#10'常量和系数可分别为0。'#13#10'如果均为0，则不使用总超时' +
          '设定。'
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
          '用于设定写总超时时间。'#13#10'写总超时时间 = (总超时系数 X 接收字符数)' +
          ' + 总超时常量'#13#10'常量和系数可分别为0。'#13#10'如果均为0，则不使用总超时' +
          '设定。'
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
    Caption = '确定(&O)'
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
    Caption = '显示提示信息'
    TabOrder = 1
    OnClick = cbShowHintClick
  end
end
