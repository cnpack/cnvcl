object Form1: TForm1
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
        TabOrder = 0
      end
      object chkGanZhi: TCheckBox
        Left = 40
        Top = 400
        Width = 97
        Height = 17
        Caption = '显示干支'
        TabOrder = 1
        OnClick = chkGanZhiClick
      end
      object chkMonthButton: TCheckBox
        Left = 160
        Top = 400
        Width = 177
        Height = 17
        Caption = '显示月份前进后退按钮'
        TabOrder = 2
        OnClick = chkMonthButtonClick
      end
      object chkYearButton: TCheckBox
        Left = 352
        Top = 400
        Width = 177
        Height = 17
        Caption = '显示年份前进后退按钮'
        TabOrder = 3
        OnClick = chkYearButtonClick
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
  end
end
