object Form1: TForm1
  Left = 548
  Top = 259
  Width = 431
  Height = 334
  Caption = '用游戏修改器搜索这几个数值？'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 32
    Top = 80
    Width = 16
    Height = 13
    Caption = 'lbl1'
  end
  object lbl2: TLabel
    Left = 264
    Top = 80
    Width = 16
    Height = 13
    Caption = 'lbl1'
  end
  object Button1: TButton
    Left = 32
    Top = 32
    Width = 113
    Height = 25
    Caption = '基类属性加一'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 264
    Top = 32
    Width = 121
    Height = 25
    Caption = '子类属性加一'
    TabOrder = 1
    OnClick = Button2Click
  end
  object mmo1: TMemo
    Left = 32
    Top = 120
    Width = 353
    Height = 145
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '宋体'
    Font.Style = []
    Lines.Strings = (
      '说明：两值均用TCnAntiCheater类实例以及其一个子类的实例的'
      '某属性保存。TCnAntiCheater 类在处理自身及其子类 published '
      '的带 Get 和 Set 方法的 Integer 属性的时候会挂接这两个方法'
      '，在读写这些属性的过程中插入一自定义变换过程，从而达到内'
      '存中属性值和对外显示不一致的效果以躲开游戏修改器的追踪。'
      ''
      '如果把CnAntiCheater单元的initialization部分的'
      'FEnableProtect := True;'
      '改为False;再重编译运行，则能搜索到这两个值。')
    ParentFont = False
    TabOrder = 2
  end
end
