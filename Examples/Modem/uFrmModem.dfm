object FrmModem: TFrmModem
  Left = 362
  Top = 199
  Width = 636
  Height = 484
  Caption = 'TCnModem DemoV1.0'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 12
  object grp1: TGroupBox
    Left = 223
    Top = 0
    Width = 405
    Height = 457
    Align = alClient
    Caption = '日志'
    TabOrder = 0
    object mmoLog: TMemo
      Left = 2
      Top = 14
      Width = 401
      Height = 441
      Align = alClient
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 223
    Height = 457
    Align = alLeft
    Caption = 'pnlLeft'
    TabOrder = 1
    object grp2: TGroupBox
      Left = 1
      Top = 1
      Width = 221
      Height = 98
      Align = alTop
      Caption = '基本设置'
      TabOrder = 0
      object lbl1: TLabel
        Left = 25
        Top = 28
        Width = 61
        Height = 13
        AutoSize = False
        Caption = 'COM口：'
      end
      object lbl2: TLabel
        Left = 25
        Top = 59
        Width = 43
        Height = 13
        AutoSize = False
        Caption = '电话：'
      end
      object edtPhone: TEdit
        Left = 80
        Top = 54
        Width = 121
        Height = 20
        TabOrder = 0
        Text = '801'
      end
      object se1: TSpinEdit
        Left = 80
        Top = 26
        Width = 45
        Height = 21
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 1
      end
    end
    object grp3: TGroupBox
      Left = 1
      Top = 99
      Width = 221
      Height = 357
      Align = alClient
      Caption = '基本操作'
      TabOrder = 1
      object lbl3: TLabel
        Left = 9
        Top = 79
        Width = 88
        Height = 13
        AutoSize = False
        Caption = '待发送数据：'
      end
      object btnDial: TButton
        Left = 54
        Top = 20
        Width = 75
        Height = 25
        Caption = '拨号'
        TabOrder = 0
        OnClick = btnDialClick
      end
      object edtSendData: TEdit
        Left = 8
        Top = 100
        Width = 200
        Height = 20
        TabOrder = 1
        Text = '@01;0001;A;B#'
      end
      object btnSend: TButton
        Left = 132
        Top = 130
        Width = 75
        Height = 25
        Caption = '发送'
        TabOrder = 2
        OnClick = btnSendClick
      end
      object btnHangUp: TButton
        Left = 136
        Top = 20
        Width = 75
        Height = 25
        Caption = '挂机'
        TabOrder = 3
        OnClick = btnHangUpClick
      end
    end
  end
  object cm1: TCnModem
    CommName = 'COM1'
    CommConfig.Outx_CtsFlow = True
    CommConfig.Outx_DsrFlow = True
    OnReceiveData = cm1ReceiveData
    Left = 11
    Top = 405
  end
end
