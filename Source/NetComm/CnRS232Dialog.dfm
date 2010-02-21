object CnRS232Dlg: TCnRS232Dlg
  Left = 238
  Top = 123
  BorderStyle = bsDialog
  Caption = 'Comm Settings'
  ClientHeight = 316
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcCommConfig: TPageControl
    Left = 8
    Top = 8
    Width = 297
    Height = 273
    ActivePage = tsNormal
    TabOrder = 0
    object tsNormal: TTabSheet
      Caption = 'General'
      object lblBaudRate: TLabel
        Left = 16
        Top = 12
        Width = 54
        Height = 13
        Caption = '&Baud Rate:'
      end
      object lblByteSize: TLabel
        Left = 16
        Top = 41
        Width = 47
        Height = 13
        Caption = 'B&yte Size:'
      end
      object lblParity: TLabel
        Left = 16
        Top = 70
        Width = 29
        Height = 13
        Caption = '&Parity:'
      end
      object lblStopBits: TLabel
        Left = 16
        Top = 98
        Width = 45
        Height = 13
        Caption = '&Stop Bits:'
      end
      object cbbBaudRate: TComboBox
        Left = 120
        Top = 8
        Width = 145
        Height = 21
        Hint = 'Max Speed of Serial Comm(bps).'
        ItemHeight = 13
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
        Height = 21
        Hint = 'Byte Size'
        Style = csDropDownList
        ItemHeight = 13
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
        Height = 21
        Hint = 'Parity Check Mode'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = ControlChanged
        Items.Strings = (
          'No Parity'
          'Odd Parity'
          'Even Parity'
          'Mark Parity'
          'Space Parity')
      end
      object cbbStopBits: TComboBox
        Left = 120
        Top = 94
        Width = 145
        Height = 21
        Hint = 'Stop Bits'
        Style = csDropDownList
        ItemHeight = 13
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
        Hint = 'when Parity Error, Replace to ASCII Char'
        Caption = 'On Parity Error, Replace to ASCII Char:'
        TabOrder = 4
        OnClick = ControlChanged
      end
      object cbIgnoreNullChar: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = 'Whether Ignore received NULL Char(ASCII 0)'
        Caption = 'Ignore &NULL Chars'
        TabOrder = 6
      end
      object seReplacedChar: TCnSpinEdit
        Left = 216
        Top = 126
        Width = 49
        Height = 22
        Hint = 'When Parity Error Occured, Replace with this Char(ASCII).'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
    end
    object tsXonXoff: TTabSheet
      Caption = 'SW Flow Ctrl'
      ImageIndex = 1
      object lblXonLimit: TLabel
        Left = 16
        Top = 87
        Width = 72
        Height = 13
        Caption = '&Xon Threshold:'
      end
      object lblXoffLimit: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 13
        Caption = 'X&off Threshold:'
      end
      object lblXonChar: TLabel
        Left = 16
        Top = 146
        Width = 80
        Height = 13
        Caption = 'Xon C&har(ASCII):'
      end
      object lblXoffChar: TLabel
        Left = 16
        Top = 176
        Width = 80
        Height = 13
        Caption = 'Xo&ff Char(ASCII):'
      end
      object cbTxContinueOnXoff: TCheckBox
        Left = 16
        Top = 56
        Width = 200
        Height = 17
        Hint = 
          'To decide whether to stop the sending after sending the Xoff cha' +
          'racter during receive buffer was already full.'#13#10'If checked, the ' +
          'sending will be continued when the quantity of bytes in the full' +
          ' filled receive buffer doesn'#39't reach the Xon threshold and the d' +
          'river will stop to receive bytes after its sending Xoff characte' +
          'r; If not, the sending will be continued when the quantity of by' +
          'tes in the cleared buffer is less than the Xon threshold and the' +
          ' driver resume the receiving after sending the Xon character.'
        Caption = 'Con&tinue sending data after Xoff'
        TabOrder = 2
      end
      object cbOutx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 32
        Width = 200
        Height = 17
        Hint = 
          'To decide whether to use Xon/Xoff flow control during sending da' +
          'ta.'#13#10'If checked, sending will be paused when receive Xoff charac' +
          'ters, and resume it after Xoff characters.'
        Caption = 'E&nable Xon/Xoff Output'
        TabOrder = 1
      end
      object cbInx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 8
        Width = 200
        Height = 17
        Hint = 
          'Whether use Xon/Xoff flow control during receiving data.'#13#10'If che' +
          'cked, a Xoff character will be sent when the receive buffer is a' +
          'lmost full and only some characters remain, which quantity equal' +
          's Xoff threshold; If not, a Xon character will be sent when ther' +
          'e are only few characters in the receive buffer, which amount eq' +
          'uals Xon threshold.'
        Caption = '&Enable Xon/Xoff input'
        TabOrder = 0
      end
      object seXonLimit: TCnSpinEdit
        Left = 136
        Top = 83
        Width = 65
        Height = 22
        Hint = 
          'The minimum count of chars in buffer before sending Xon.'
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
        Height = 22
        Hint = 
          'ASCII Code of Xon in Sending or Receiving, which means resume an' +
          'd continue.'
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
        Height = 22
        Hint = 'ASCII Code of Xoff in Sending or Receiving, which means Pause.'
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
        Height = 22
        Hint = 
          'The maximum count of chars in buffer before sending Xoff.'#13#10'Buffe' +
          'rn length - this maximum count = maximun chars in receive.'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnExit = seXonLimitExit
      end
    end
    object tsHardware: TTabSheet
      Caption = 'HW Flow Ctrl'
      ImageIndex = 2
      object lblDtrControl: TLabel
        Left = 16
        Top = 39
        Width = 87
        Height = 13
        Caption = '&DTR Flow Control:'
      end
      object lblRtsControl: TLabel
        Left = 16
        Top = 68
        Width = 86
        Height = 13
        Caption = '&RTS Flow Control:'
      end
      object lblInCtrl: TLabel
        Left = 16
        Top = 16
        Width = 63
        Height = 13
        Caption = '&Input Control:'
      end
      object lblOutCtrl: TLabel
        Left = 16
        Top = 108
        Width = 71
        Height = 13
        Caption = '&Output Control:'
      end
      object cbOutx_CtsFlow: TCheckBox
        Left = 16
        Top = 152
        Width = 225
        Height = 17
        Hint = 'CTS (Clear To Send)'
        Caption = 'U&se CTS Signal to Control Output Flow'
        TabOrder = 3
      end
      object cbOutx_DsrFlow: TCheckBox
        Left = 16
        Top = 128
        Width = 225
        Height = 17
        Hint = 'DSR (Data Set Ready)'
        Caption = '&Use DSR Signal to Control Output Flow'
        TabOrder = 2
      end
      object cbDsrSensitivity: TCheckBox
        Left = 16
        Top = 176
        Width = 225
        Height = 17
        Hint = 
          'Whether the transmission driver is sensitive to DSR signals.'#13#10'If' +
          ' checked, when the Modem DSR input pin is low, the driver will i' +
          'gnore any receiving bytes.'
        Caption = 'DSR S&ensibility'
        TabOrder = 4
      end
      object cbbDtrControl: TComboBox
        Left = 120
        Top = 35
        Width = 145
        Height = 21
        Hint = 'Use DTR (Data Terminal Ready) Signal to Control the Flow'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Enable DTR Line and Hold'
          'Disable DTR Line and Hold'
          'Enable DTR Handshake')
      end
      object cbbRtsControl: TComboBox
        Left = 120
        Top = 64
        Width = 145
        Height = 21
        Hint = 'Use RTS (Request to Send) Signal to Control the Flow'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'Enable RTS and Hold'
          'Disable RTS and Hold'
          'Enable RTS Handshake'
          'Use Burst Mode')
      end
    end
    object tsTimeouts: TTabSheet
      Caption = 'Timeout'
      ImageIndex = 3
      object lblReadIntervalTimeout: TLabel
        Left = 16
        Top = 15
        Width = 122
        Height = 13
        Caption = '&Reading Timeout Interval:'
      end
      object lblReadTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 45
        Width = 155
        Height = 13
        Caption = 'Total Reading &Timeout Multiplier:'
      end
      object lblMSec1: TLabel
        Left = 240
        Top = 16
        Width = 27
        Height = 13
        Caption = 'mSec'
      end
      object lblMSec2: TLabel
        Left = 240
        Top = 46
        Width = 27
        Height = 13
        Caption = 'mSec'
      end
      object lblReadTotalTimeoutConstant: TLabel
        Left = 16
        Top = 75
        Width = 156
        Height = 13
        Caption = 'Total Re&ading Timeout Constant:'
      end
      object lblMSec3: TLabel
        Left = 240
        Top = 76
        Width = 27
        Height = 13
        Caption = 'mSec'
      end
      object lblWriteTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 105
        Width = 151
        Height = 13
        Caption = 'Total  &Writing Timeout Multiplier:'
      end
      object lblMSec4: TLabel
        Left = 240
        Top = 106
        Width = 27
        Height = 13
        Caption = 'mSec'
      end
      object lblWriteTotalTimeoutConstant: TLabel
        Left = 16
        Top = 135
        Width = 149
        Height = 13
        Caption = 'Total Wr&iting Timeout Constant:'
      end
      object lblMSec5: TLabel
        Left = 240
        Top = 136
        Width = 27
        Height = 13
        Caption = 'mSec'
      end
      object seReadIntervalTimeout: TCnSpinEdit
        Left = 176
        Top = 11
        Width = 57
        Height = 22
        Hint = 
          'The maximum time between 2 consecutive chars in serial line.'#13#10'If' +
          ' the recieving interval of 2 consecutive chars exceeds this valu' +
          'e, '#13#10'reading operation will finish and the buffer data will be r' +
          'eturn.'#13#10'0 means no timeout.'#13#10
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object seReadTotalTimeoutMultiplier: TCnSpinEdit
        Left = 176
        Top = 41
        Width = 57
        Height = 22
        Hint = 
          'Used to calculate the Total reading timeout value.'#13#10#13#10'Total read' +
          'ing timeout value = (Total Reading Timeout Multiplier * chars co' +
          'unt) + Total reading timeout constant'#13#10'Both 0 means disabled.'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object seReadTotalTimeoutConstant: TCnSpinEdit
        Left = 176
        Top = 71
        Width = 57
        Height = 22
        Hint = 
          'Used to calculate the Total reading timeout value.'#13#10#13#10'Total read' +
          'ing timeout value = (Total Reading Timeout Multiplier * chars co' +
          'unt) + Total reading timeout constant'#13#10'Both 0 means disabled.'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object seWriteTotalTimeoutMultiplier: TCnSpinEdit
        Left = 176
        Top = 101
        Width = 57
        Height = 22
        Hint = 
          'Used to calculate the Total Writing Timeout value.'#13#10#13#10'Total writ' +
          'ing timeout value = (Total writing Timeout Multiplier * chars co' +
          'unt) + Total writing timeout constant'#13#10'Both 0 means disabled.'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object seWriteTotalTimeoutConstant: TCnSpinEdit
        Left = 176
        Top = 131
        Width = 57
        Height = 22
        Hint = 
          'Used to calculate the Total Writing Timeout value.'#13#10#13#10'Total writ' +
          'ing timeout value = (Total writing Timeout Multiplier * chars co' +
          'unt) + Total writing timeout constant'#13#10'Both 0 means disabled.'
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
    Caption = '&OK'
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
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cbShowHint: TCheckBox
    Left = 8
    Top = 290
    Width = 105
    Height = 17
    Caption = 'Show Hint'
    TabOrder = 1
    OnClick = cbShowHintClick
  end
end
