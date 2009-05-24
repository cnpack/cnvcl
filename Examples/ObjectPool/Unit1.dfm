object Form1: TForm1
  Left = 285
  Top = 67
  Width = 401
  Height = 543
  Caption = 'ObjectPool and TADOConPool test suite'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Splitter3: TSplitter
    Left = 0
    Top = 386
    Width = 393
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 393
    Height = 386
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = '对象池测试'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 0
        Top = 165
        Width = 385
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 385
        Height = 165
        Align = alTop
        Lines.Strings = (
          'Memo2')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Memo1: TMemo
        Left = 0
        Top = 168
        Width = 385
        Height = 107
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object Panel1: TPanel
        Left = 0
        Top = 275
        Width = 385
        Height = 84
        Align = alBottom
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object Label1: TLabel
          Left = 8
          Top = 12
          Width = 72
          Height = 12
          Caption = '对象获取策略'
        end
        object Label2: TLabel
          Left = 8
          Top = 36
          Width = 72
          Height = 12
          Caption = '无空闲对象时'
        end
        object Label3: TLabel
          Left = 8
          Top = 60
          Width = 72
          Height = 12
          Caption = '线到达峰值时'
        end
        object Label4: TLabel
          Left = 160
          Top = 60
          Width = 60
          Height = 12
          Caption = '引用数峰值'
        end
        object Label5: TLabel
          Left = 160
          Top = 36
          Width = 36
          Height = 12
          Caption = '对象数'
        end
        object Label12: TLabel
          Left = 260
          Top = 60
          Width = 24
          Height = 12
          Caption = '选项'
        end
        object ComboBox1: TComboBox
          Left = 84
          Top = 8
          Width = 169
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 0
          OnChange = ComboBox1Change
          Items.Strings = (
            '引用数小优先'
            '引用数小，工作效率高优先'
            '引用数小，工作次数少优先'
            '引用数小，工作次数多优先'
            '引用数小，工作时间少优先'
            '引用数小，工作时间多优先'
            '无引用，工作效率高优先'
            '无引用，工作次数少优先'
            '无引用，工作次数多优先'
            '无引用，工作时间少优先'
            '无引用，工作时间多优先')
        end
        object ComboBox2: TComboBox
          Left = 84
          Top = 32
          Width = 73
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 1
          OnChange = ComboBox2Change
          Items.Strings = (
            '等待可用'
            '获取失败'
            '重用对象')
        end
        object ComboBox3: TComboBox
          Left = 84
          Top = 56
          Width = 73
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 4
          OnChange = ComboBox3Change
          Items.Strings = (
            '等待可用'
            '获取失败')
        end
        object SpinEdit1: TSpinEdit
          Left = 224
          Top = 56
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = SpinEdit1Change
        end
        object SpinEdit2: TSpinEdit
          Left = 196
          Top = 32
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = SpinEdit2Change
        end
        object SpinEdit3: TSpinEdit
          Left = 224
          Top = 32
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = SpinEdit3Change
        end
        object Button1: TButton
          Left = 260
          Top = 8
          Width = 75
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Caption = '加入任务'
          TabOrder = 6
          OnClick = Button1Click
        end
        object SpinEdit4: TSpinEdit
          Left = 336
          Top = 8
          Width = 41
          Height = 21
          Anchors = [akTop, akRight]
          MaxLength = 3
          MaxValue = 999
          MinValue = 0
          TabOrder = 7
          Value = 10
        end
        object CheckBox1: TCheckBox
          Left = 260
          Top = 34
          Width = 73
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = '每200ms'
          TabOrder = 8
        end
        object SpinEdit6: TSpinEdit
          Left = 336
          Top = 32
          Width = 41
          Height = 21
          Anchors = [akTop, akRight]
          MaxLength = 3
          MaxValue = 999
          MinValue = 0
          TabOrder = 9
          Value = 10
        end
        object ComboBox8: TComboBox
          Left = 288
          Top = 56
          Width = 89
          Height = 20
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 12
          TabOrder = 10
          Items.Strings = (
            '无特殊选项'
            '需要初始化'
            '对象需独用'
            '随机设定')
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = '连接池测试'
      ImageIndex = 2
      object Splitter2: TSplitter
        Left = 0
        Top = 165
        Width = 385
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 385
        Height = 165
        Align = alTop
        Lines.Strings = (
          'Memo3')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Memo4: TMemo
        Left = 0
        Top = 168
        Width = 385
        Height = 81
        Align = alClient
        Lines.Strings = (
          'Memo4')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object Panel2: TPanel
        Left = 0
        Top = 249
        Width = 385
        Height = 110
        Align = alBottom
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object Label6: TLabel
          Left = 8
          Top = 12
          Width = 72
          Height = 12
          Caption = '对象获取策略'
        end
        object Label7: TLabel
          Left = 8
          Top = 36
          Width = 72
          Height = 12
          Caption = '无空闲对象时'
        end
        object Label8: TLabel
          Left = 8
          Top = 60
          Width = 72
          Height = 12
          Caption = '线到达峰值时'
        end
        object Label9: TLabel
          Left = 160
          Top = 60
          Width = 60
          Height = 12
          Caption = '引用数峰值'
        end
        object Label10: TLabel
          Left = 160
          Top = 36
          Width = 36
          Height = 12
          Caption = '对象数'
        end
        object Label11: TLabel
          Left = 260
          Top = 60
          Width = 24
          Height = 12
          Caption = '选项'
        end
        object Label13: TLabel
          Left = 8
          Top = 84
          Width = 72
          Height = 12
          Caption = '低负载引用数'
        end
        object Label14: TLabel
          Left = 120
          Top = 84
          Width = 18
          Height = 12
          Caption = 'SQL'
        end
        object ComboBox4: TComboBox
          Left = 84
          Top = 8
          Width = 169
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 0
          OnChange = ComboBox4Change
          Items.Strings = (
            '引用数小优先'
            '引用数小，工作效率高优先'
            '引用数小，工作次数少优先'
            '引用数小，工作次数多优先'
            '引用数小，工作时间少优先'
            '引用数小，工作时间多优先'
            '无引用，工作效率高优先'
            '无引用，工作次数少优先'
            '无引用，工作次数多优先'
            '无引用，工作时间少优先'
            '无引用，工作时间多优先')
        end
        object ComboBox5: TComboBox
          Left = 84
          Top = 32
          Width = 73
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 1
          OnChange = ComboBox5Change
          Items.Strings = (
            '等待可用'
            '获取失败'
            '重用对象')
        end
        object ComboBox6: TComboBox
          Left = 84
          Top = 56
          Width = 73
          Height = 20
          Style = csDropDownList
          ItemHeight = 12
          TabOrder = 4
          OnChange = ComboBox6Change
          Items.Strings = (
            '等待可用'
            '获取失败')
        end
        object SpinEdit5: TSpinEdit
          Left = 224
          Top = 56
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = SpinEdit5Change
        end
        object SpinEdit7: TSpinEdit
          Left = 196
          Top = 32
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = SpinEdit7Change
        end
        object SpinEdit8: TSpinEdit
          Left = 224
          Top = 32
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = SpinEdit8Change
        end
        object Button2: TButton
          Left = 260
          Top = 8
          Width = 75
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Caption = '加入任务'
          TabOrder = 8
          OnClick = Button2Click
        end
        object SpinEdit9: TSpinEdit
          Left = 336
          Top = 8
          Width = 41
          Height = 21
          Anchors = [akTop, akRight]
          MaxLength = 3
          MaxValue = 999
          MinValue = 0
          TabOrder = 9
          Value = 10
        end
        object CheckBox2: TCheckBox
          Left = 260
          Top = 34
          Width = 73
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = '每200ms'
          TabOrder = 10
        end
        object SpinEdit10: TSpinEdit
          Left = 336
          Top = 32
          Width = 41
          Height = 21
          Anchors = [akTop, akRight]
          MaxLength = 3
          MaxValue = 999
          MinValue = 0
          TabOrder = 11
          Value = 10
        end
        object Edit1: TEdit
          Left = 140
          Top = 80
          Width = 113
          Height = 20
          TabOrder = 7
          Text = 'SELECT COUNT(*) FROM dual'
        end
        object Edit2: TEdit
          Left = 260
          Top = 80
          Width = 93
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 13
        end
        object Button3: TButton
          Left = 356
          Top = 80
          Width = 21
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '…'
          TabOrder = 14
          OnClick = Button3Click
        end
        object ComboBox7: TComboBox
          Left = 288
          Top = 56
          Width = 89
          Height = 20
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 12
          TabOrder = 12
          Items.Strings = (
            '无特殊选项'
            '需要初始化'
            '对象需独用')
        end
        object SpinEdit11: TSpinEdit
          Left = 84
          Top = 80
          Width = 29
          Height = 21
          MaxLength = 1
          MaxValue = 9
          MinValue = 0
          TabOrder = 6
          Value = 0
          OnChange = SpinEdit11Change
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 389
    Width = 393
    Height = 127
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 1
    object Memo5: TListBox
      Left = 0
      Top = 25
      Width = 393
      Height = 102
      Align = alClient
      ItemHeight = 12
      TabOrder = 0
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 393
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SpeedButton1: TSpeedButton
        Left = 4
        Top = 1
        Width = 89
        Height = 22
        Caption = '清空输出记录'
        OnClick = SpeedButton1Click
      end
      object CheckBox3: TCheckBox
        Left = 96
        Top = 4
        Width = 97
        Height = 17
        Caption = '显示输出记录'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
  end
  object CnThreadPool1: TCnThreadPool
    AdjustInterval = 5000
    DeadTaskAsNew = False
    MinAtLeast = True
    ThreadDeadTimeout = 3000
    ThreadsMinCount = 20
    ThreadsMaxCount = 100
    OnProcessRequest = CnThreadPool1ProcessRequest
    Left = 120
    Top = 40
  end
  object CnObjectPool1: TCnObjectPool
    MinSize = 3
    MaxSize = 5
    LowLoadCount = 0
    PeakCount = 9
    Left = 188
    Top = 40
  end
  object CnADOConPool1: TCnADOConPool
    ConnectionString = 
      'Provider=MSDAORA.1;Password=;User ID=user;Data Source=localhost;' +
      'Persist Security Info=True'
    MinSize = 1
    MaxSize = 8
    LowLoadCount = 2
    PeakCount = 4
    WaitTimeOut = 0
    OnReInitOne = CnADOConPool1ReInitOne
    Left = 256
    Top = 40
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 188
    Top = 95
  end
  object CnThreadPool2: TCnThreadPool
    AdjustInterval = 5000
    DeadTaskAsNew = False
    MinAtLeast = True
    ThreadsMinCount = 20
    ThreadsMaxCount = 100
    OnProcessRequest = CnThreadPool2ProcessRequest
    Left = 120
    Top = 99
  end
end
