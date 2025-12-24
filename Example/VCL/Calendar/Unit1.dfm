object FormCalendar: TFormCalendar
  Left = 273
  Top = 110
  BorderStyle = bsDialog
  Caption = 'CnCalendar 测试程序'
  ClientHeight = 573
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object pgc1: TPageControl
    Left = 10
    Top = 8
    Width = 545
    Height = 553
    ActivePage = ts1
    TabOrder = 0
    object ts1: TTabSheet
      Caption = '公历'
      object lblYear: TLabel
        Left = 8
        Top = 20
        Width = 12
        Height = 12
        Caption = '年'
      end
      object lblYue: TLabel
        Left = 112
        Top = 20
        Width = 12
        Height = 12
        Caption = '月'
      end
      object lblDay: TLabel
        Left = 208
        Top = 20
        Width = 12
        Height = 12
        Caption = '日'
      end
      object lblHour: TLabel
        Left = 296
        Top = 20
        Width = 12
        Height = 12
        Caption = '时'
      end
      object mmoResult: TMemo
        Left = 0
        Top = 64
        Width = 537
        Height = 462
        Align = alBottom
        ScrollBars = ssVertical
        TabOrder = 6
      end
      object edtYear: TEdit
        Left = 32
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 0
        Text = '2005'
      end
      object edtMonth: TEdit
        Left = 128
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 1
        Text = '12'
      end
      object edtDay: TEdit
        Left = 224
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 2
        Text = '29'
      end
      object btnCalc: TButton
        Left = 384
        Top = 16
        Width = 65
        Height = 21
        Caption = '一算！'
        TabOrder = 4
        OnClick = btnCalcClick
      end
      object edtHour: TEdit
        Left = 312
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 3
        Text = '0'
      end
      object Button1: TButton
        Left = 464
        Top = 16
        Width = 65
        Height = 21
        Caption = '二算！'
        TabOrder = 5
        OnClick = Button1Click
      end
    end
    object tsCalendar: TTabSheet
      Caption = '月历'
      ImageIndex = 1
      object lblDate: TLabel
        Left = 280
        Top = 400
        Width = 6
        Height = 12
      end
      object CnMonthCalendar1: TCnMonthCalendar
        Left = 32
        Top = 32
        Width = 457
        Height = 289
        CalColors.DaySelectTextColor = clWhite
        ShowMonthButton = True
        ShowYearButton = True
        Date = 39604
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = '宋体'
        Font.Style = []
        OnChange = CnMonthCalendar1Change
        TabOrder = 0
      end
      object chkGanZhi: TCheckBox
        Left = 40
        Top = 352
        Width = 97
        Height = 17
        Caption = '显示干支'
        TabOrder = 1
        OnClick = chkGanZhiClick
      end
      object chkMonthButton: TCheckBox
        Left = 160
        Top = 352
        Width = 177
        Height = 17
        Caption = '显示月份前进后退按钮'
        TabOrder = 2
        OnClick = chkMonthButtonClick
      end
      object chkYearButton: TCheckBox
        Left = 352
        Top = 352
        Width = 177
        Height = 17
        Caption = '显示年份前进后退按钮'
        TabOrder = 3
        OnClick = chkYearButtonClick
      end
      object dtpSet: TDateTimePicker
        Left = 32
        Top = 400
        Width = 186
        Height = 20
        CalAlignment = dtaLeft
        Date = 42431.4619409954
        Time = 42431.4619409954
        DateFormat = dfShort
        DateMode = dmComboBox
        Kind = dtkDate
        ParseInput = False
        TabOrder = 4
        OnChange = dtpSetChange
      end
    end
    object tsConvert: TTabSheet
      Caption = '农历转公历'
      ImageIndex = 2
      object lbl1: TLabel
        Left = 8
        Top = 20
        Width = 12
        Height = 12
        Caption = '年'
      end
      object lbl2: TLabel
        Left = 112
        Top = 20
        Width = 12
        Height = 12
        Caption = '月'
      end
      object lbl3: TLabel
        Left = 208
        Top = 20
        Width = 12
        Height = 12
        Caption = '日'
      end
      object edtLunarYear: TEdit
        Left = 32
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 0
        Text = '2009'
      end
      object edtLunarMonth: TEdit
        Left = 128
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 1
        Text = '5'
      end
      object edtLunarDay: TEdit
        Left = 224
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 2
        Text = '14'
      end
      object chkLeap: TCheckBox
        Left = 296
        Top = 18
        Width = 73
        Height = 17
        Caption = '闰月'
        TabOrder = 4
        OnClick = chkGanZhiClick
      end
      object btnCalcLunar: TButton
        Left = 384
        Top = 16
        Width = 65
        Height = 21
        Caption = '算！'
        TabOrder = 3
        OnClick = btnCalcLunarClick
      end
      object mmoLunar: TMemo
        Left = 0
        Top = 64
        Width = 537
        Height = 462
        Align = alBottom
        ScrollBars = ssVertical
        TabOrder = 5
      end
    end
    object tsSun: TTabSheet
      Caption = '日出日落计算'
      ImageIndex = 3
      object lblSunYear: TLabel
        Left = 8
        Top = 20
        Width = 12
        Height = 12
        Caption = '年'
      end
      object lblSunDay: TLabel
        Left = 208
        Top = 20
        Width = 12
        Height = 12
        Caption = '日'
      end
      object lblSunMonth: TLabel
        Left = 112
        Top = 20
        Width = 12
        Height = 12
        Caption = '月'
      end
      object lblLongi: TLabel
        Left = 296
        Top = 20
        Width = 12
        Height = 12
        Caption = '经'
      end
      object lblLat: TLabel
        Left = 392
        Top = 20
        Width = 12
        Height = 12
        Caption = '纬'
      end
      object edtSunYear: TEdit
        Left = 32
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 0
        Text = '2025'
      end
      object edtSunMonth: TEdit
        Left = 128
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 1
        Text = '2'
      end
      object edtSunDay: TEdit
        Left = 224
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 2
        Text = '20'
      end
      object edtSunLongi: TEdit
        Left = 320
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 3
        Text = '121.47493'
      end
      object edtSunLat: TEdit
        Left = 416
        Top = 16
        Width = 57
        Height = 20
        TabOrder = 4
        Text = '31.22897'
      end
      object btnSunTime: TButton
        Left = 32
        Top = 48
        Width = 75
        Height = 25
        Caption = '时刻'
        TabOrder = 5
        OnClick = btnSunTimeClick
      end
      object btnSunAngle: TButton
        Left = 128
        Top = 48
        Width = 75
        Height = 25
        Caption = '方位角'
        Enabled = False
        TabOrder = 6
        OnClick = btnSunAngleClick
      end
      object mmoSun: TMemo
        Left = 0
        Top = 88
        Width = 537
        Height = 438
        Align = alBottom
        ScrollBars = ssVertical
        TabOrder = 7
      end
    end
    object tsDays: TTabSheet
      Caption = '日数计算'
      ImageIndex = 4
      object btnEquStandardDays: TButton
        Left = 128
        Top = 16
        Width = 121
        Height = 25
        Caption = '反向检查等效标准日'
        TabOrder = 1
        OnClick = btnEquStandardDaysClick
      end
      object btnJulianDays: TButton
        Left = 352
        Top = 16
        Width = 97
        Height = 25
        Caption = '反向检查儒略日'
        TabOrder = 3
        OnClick = btnJulianDaysClick
      end
      object btnCheckDays: TButton
        Left = 264
        Top = 16
        Width = 75
        Height = 25
        Caption = '检查儒略日'
        TabOrder = 2
        OnClick = btnCheckDaysClick
      end
      object btnEquStandardDays1: TButton
        Left = 16
        Top = 16
        Width = 105
        Height = 25
        Caption = '检查等效标准日'
        TabOrder = 0
        OnClick = btnEquStandardDays1Click
      end
      object mmoDays: TMemo
        Left = 16
        Top = 96
        Width = 505
        Height = 417
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 10
        WordWrap = False
      end
      object btnCheckLunar: TButton
        Left = 456
        Top = 16
        Width = 75
        Height = 25
        Caption = '检查农历'
        TabOrder = 4
        OnClick = btnCheckLunarClick
      end
      object btnVerifyLeapNumber: TButton
        Left = 16
        Top = 56
        Width = 105
        Height = 25
        Caption = '检查预置闰月数据'
        TabOrder = 5
        OnClick = btnVerifyLeapNumberClick
      end
      object btnCheckAll: TButton
        Left = 128
        Top = 56
        Width = 121
        Height = 25
        Caption = '检查多个日'
        TabOrder = 6
        OnClick = btnCheckAllClick
      end
      object btnCheckFloatJieQi: TButton
        Left = 264
        Top = 56
        Width = 75
        Height = 25
        Caption = '检查多个节气'
        TabOrder = 7
        OnClick = btnCheckFloatJieQiClick
      end
      object btnCheckStep: TButton
        Left = 352
        Top = 56
        Width = 97
        Height = 25
        Caption = '检查日期步进'
        TabOrder = 8
        OnClick = btnCheckStepClick
      end
      object btnGenCal: TButton
        Left = 456
        Top = 56
        Width = 75
        Height = 25
        Caption = '生成历表'
        TabOrder = 9
        OnClick = btnGenCalClick
      end
    end
  end
end
