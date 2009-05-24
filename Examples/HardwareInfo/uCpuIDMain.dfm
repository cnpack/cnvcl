object FrmCPUIDs: TFrmCPUIDs
  Left = 177
  Top = 201
  Width = 544
  Height = 375
  Caption = 'TCnCpuId Demo 获取多核CPU指定的序列号'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object BtnGetCpuIDs: TButton
    Left = 16
    Top = 16
    Width = 121
    Height = 25
    Caption = '获得CPU序列号'
    TabOrder = 1
    OnClick = BtnGetCpuIDsClick
  end
  object Memo1: TMemo
    Left = 150
    Top = 0
    Width = 386
    Height = 348
    Align = alRight
    ImeName = '中文 (简体) - 微软拼音'
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnGetUsage: TButton
    Left = 16
    Top = 112
    Width = 121
    Height = 25
    Caption = '获得 CPU 占用率'
    TabOrder = 4
    OnClick = btnGetUsageClick
  end
  object btnGetCpuOems: TButton
    Left = 16
    Top = 80
    Width = 121
    Height = 25
    Caption = '获得CPU生产厂商'
    TabOrder = 3
    OnClick = btnGetCpuOemsClick
  end
  object btnInfoStr: TButton
    Left = 16
    Top = 48
    Width = 121
    Height = 25
    Caption = '获得CPU信息串'
    TabOrder = 2
    OnClick = btnInfoStrClick
  end
  object btnGetBios: TButton
    Left = 16
    Top = 144
    Width = 121
    Height = 21
    Caption = '获得 BIOS ID'
    TabOrder = 5
    OnClick = btnGetBiosClick
  end
end
