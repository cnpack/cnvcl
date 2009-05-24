object Form1: TForm1
  Left = 252
  Top = 226
  Width = 586
  Height = 365
  Caption = 'IOCP and MEMORYPOOL Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 368
    Top = 16
    Width = 75
    Height = 25
    Caption = 'UDP Test'
    TabOrder = 0
    OnClick = btn1Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 56
    Width = 505
    Height = 257
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btn2: TButton
    Left = 136
    Top = 16
    Width = 75
    Height = 25
    Caption = 'TCP Test'
    TabOrder = 2
    OnClick = btn2Click
  end
  object idpclnt1: TIdUDPClient
    Port = 0
    Left = 464
    Top = 24
  end
  object idpsrvr1: TIdUDPServer
    Bindings = <>
    DefaultPort = 9001
    Left = 464
    Top = 72
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 9000
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnExecute = IdTCPServer1Execute
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 80
    Top = 72
  end
  object IdTCPClient1: TIdTCPClient
    MaxLineAction = maException
    ReadTimeout = -1
    Port = 0
    Left = 80
    Top = 32
  end
  object cncpsmplmpl1: TCnIocpSimpleMemPool
    BlockMaxCount = 20
    Left = 232
    Top = 152
  end
  object cncpscktdptr1: TCnIocpSocketAdapter
    MemoryPool = cncpsmplmpl1
    OnSendEvent = cncpscktdptr1SendEvent
    OnRecvEvent = cncpscktdptr1RecvEvent
    OnSendToEvent = cncpscktdptr1SendToEvent
    OnRecvFromEvent = cncpscktdptr1RecvFromEvent
    Left = 304
    Top = 152
  end
end
