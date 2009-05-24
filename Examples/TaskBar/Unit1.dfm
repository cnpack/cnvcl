object Form1: TForm1
  Left = 219
  Top = 183
  Width = 363
  Height = 486
  Caption = '任务栏'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 355
    Height = 459
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = '托盘区操作'
      object Image2: TImage
        Left = 234
        Top = 5
        Width = 18
        Height = 18
      end
      object CheckListBox1: TCheckListBox
        Left = 0
        Top = 32
        Width = 343
        Height = 361
        OnClickCheck = CheckListBox1ClickCheck
        ImeName = '中文 (简体) - 谷歌拼音输入法'
        ItemHeight = 12
        TabOrder = 0
        OnClick = CheckListBox1Click
        OnDblClick = CheckListBox1DblClick
      end
      object RadioButton2: TRadioButton
        Left = 0
        Top = 400
        Width = 153
        Height = 17
        Caption = '不显示默认系统隐藏图标'
        TabOrder = 1
        OnClick = RadioButton2Click
      end
      object RadioButton1: TRadioButton
        Left = 160
        Top = 399
        Width = 185
        Height = 17
        Caption = '显示全部（包括系统隐藏图标）'
        TabOrder = 2
        OnClick = RadioButton2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = '任务栏引用程序区操作'
      ImageIndex = 1
      object CheckListBox2: TCheckListBox
        Left = 0
        Top = 36
        Width = 343
        Height = 361
        OnClickCheck = CheckListBox2ClickCheck
        ImeName = '中文 (简体) - 谷歌拼音输入法'
        ItemHeight = 12
        TabOrder = 0
        OnDblClick = CheckListBox2DblClick
      end
      object RadioButton3: TRadioButton
        Left = -2
        Top = 405
        Width = 153
        Height = 17
        Caption = '不显示默认系统隐藏图标'
        TabOrder = 1
        OnClick = RadioButton3Click
      end
      object RadioButton4: TRadioButton
        Left = 158
        Top = 404
        Width = 185
        Height = 17
        Caption = '显示全部（包括系统隐藏图标）'
        TabOrder = 2
        OnClick = RadioButton3Click
      end
      object Button0: TButton
        Left = 192
        Top = 8
        Width = 137
        Height = 25
        Caption = '隐藏任务栏上的所有'
        TabOrder = 3
        OnClick = Button0Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = '开始按扭操作'
      ImageIndex = 2
      object Button1: TButton
        Left = 96
        Top = 48
        Width = 139
        Height = 25
        Caption = '修改开始按扭名称'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 96
        Top = 88
        Width = 139
        Height = 25
        Caption = '隐藏开始按扭'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 96
        Top = 128
        Width = 139
        Height = 25
        Caption = '使开始按扭不可用'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 96
        Top = 168
        Width = 139
        Height = 25
        Caption = '隐藏任务栏'
        TabOrder = 3
        OnClick = Button4Click
      end
    end
  end
end
