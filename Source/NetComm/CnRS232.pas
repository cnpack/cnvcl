{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{                                                                              }
{  该串口通讯组件修改自 小猪工作室 Small-Pig Team （中国台湾）的               }
{  SPCOMM V2.5 串口通讯组件，以下是原单元的原始声明：                          }
{                                                                              }
{  硂琌梆硄癟じン, ㄑ Delphi 2.0 莱ノ祘Αㄏノ. 続ノㄓ暗穨北の    }
{  虏虫肚块. じン㊣ Win32 API ㄓ笷Θ┮惠, 叫ǎCommunications场     }
{                                                                              }
{  じン把σ David Wann. ┮籹 COMM32.PAS Version 1.0﹍弧      }
{  This Communications Component is implemented using separate Read and Write  }
{  threads. Messages from the threads are posted to the Comm control which is  }
{  an invisible window. To handle data from the comm port, simply              }
{  attach a handler to 'OnReceiveData'. There is no need to free the memory    }
{  buffer passed to this handler. If TAPI is used to open the comm port, some  }
{  changes to this component are needed ('StartComm' currently opens the comm  }
{  port). The 'OnRequestHangup' event is included to assist this.              }
{                                                                              }
{  David Wann                                                                  }
{  Stamina Software                                                            }
{  28/02/96                                                                    }
{  davidwann@hunterlink.net.au                                                 }
{                                                                              }
{                                                                              }
{  硂じンЧ禣, 舧ī' э┪暗ヴㄤウノ硚. 埃虫縒砪芥じン.       }
{  This component is totally free(copyleft), you can do anything in any        }
{  purpose EXCEPT SELL IT ALONE.                                               }
{                                                                              }
{                                                                              }
{  Author?: 睫 Small-Pig Team         in Taiwan R.O.C.                }
{  Email   : spigteam@vlsi.ice.cycu.edu.tw                                     }
{  Date ? : 1997/5/9                                                          }
{                                                                              }
{  Version 1.01   1996/9/4                                                     }
{                 - Add setting Parity, Databits, StopBits                     }
{                 - Add setting Flowcontrol:Dtr-Dsr, Cts-Rts, Xon-Xoff         }
{                 - Add setting Timeout information for read/write             }
{                                                                              }
{  Version 1.02   1996/12/24                                                   }
{                 - Add Sender parameter to TReceiveDataEvent                  }
{                                                                              }
{  Version 2.0    1997/4/15                                                    }
{                 - Support separatly DTR/DSR and RTS/CTS hardware flow        }
{                   control setting                                            }
{                 - Support separatly OutX and InX software flow control       }
{                   setting                                                    }
{                 - Log file(for debug) may used by many comms at the same     }
{                   time                                                       }
{                 - Add DSR sensitivity property                               }
{                 - You can set error char. replacement when parity error      }
{                 - Let XonLim/XoffLim and XonChar/XoffChar setting by         }
{                   yourself                                                   }
{                 - You may change flow-control when comm is still opened      }
{                 - Change TComm32 to TComm                                    }
{                 - Add OnReceiveError event handler                           }
{                 - Add OnReceiveError event handler when overrun, framing     }
{                   error, parity error                                        }
{                 - Fix some bug                                               }
{                                                                              }
{  Version 2.01   1997/4/19                                                    }
{                 - Support some property for modem                            }
{                 - Add OnModemStateChange event hander when RLSD(CD) change   }
{                   state                                                      }
{                                                                              }
{  Version 2.02   1997/4/28                                                    }
{                 - Bug fix: When receive XOFF character, the system           }
{                   FAULT!!!!                                                  }
{                                                                              }
{  Version 2.5    1997/5/9                                                     }
{                 - Add OnSendDataEmpty event handler when all data in buffer  }
{                   are sent(send-buffer become empty) this handler is called. }
{                   You may call send data here.                               }
{                 - Change the ModemState parameters in OnModemStateChange     }
{                   to ModemEvent to indicate what modem event make this call  }
{                 - Add RING signal detect. When RLSD changed state or RING    }
{                   signal was detected, OnModemStateChange handler is called  }
{                 - Change XonLim and XoffLim from 100 to 500                  }
{                 - Remove TWriteThread.WriteData member                       }
{                 - PostHangupCall is re-design for debuging function          }
{                 - Add a boolean property SendDataEmpty, True when send       }
{                   buffer is empty                                            }
{                                                                              }
{******************************************************************************}

unit CnRS232;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：CnRS232串口通讯组件单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：CnRS232串口通讯组件直接由 小猪工作室 Small-Pig Team （中国台湾）
*           spigteam@vlsi.ice.cycu.edu.tw
*           的 SPCOMM V2.5 串口通讯组件修改而来。
*           而 SPCOMM 又是由 David Wann (Stamina Software)
*           davidwann@hunterlink.net.au
*           提供的 COMM32.PAS Version 1.0 修改而来。
*           CnRS232.pas 单元保留了以上内容的详细说明，请查阅。
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2012.03.22 V1.2
*                修正一处读入数据成功但长度为0而退出的问题，感谢大雄
*           2008.11.17 V1.1
*                增加 D2009 支持后修正问题，感谢大雄
*           2002.04.08 V1.0
*                创建单元，增加注释
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, CnClasses, CnConsts, CnNetConsts;

const
  PWM_GOTCOMMDATA = WM_USER + 1;
  PWM_RECEIVEERROR = WM_USER + 2;
  PWM_REQUESTHANGUP = WM_USER + 3;
  PWM_MODEMSTATECHANGE = WM_USER + 4;
  PWM_SENDDATAEMPTY = WM_USER + 5;
  PWM_COMMWRITE = WM_USER + 1;
  ME_CTS = 1;
  ME_DSR = 2;
  ME_RING = 4;
  ME_RLSD = 8;

type
  TParity = (paNone, paOdd, paEven, paMark, paSpace);
  {* 串口通讯奇偶校验方式
   |<PRE>
     paNone:            - 无校验
     paOdd:             - 奇校验方式
     paEven:            - 偶校验方式
     paMark:            - 传号校验方式
     paSpace:           - 空号校验方式
   |</PRE>}
  TStopBits = (_1, _1_5, _2);
  {* 串口通讯停止位长度
   |<PRE>
     _1:                - 1位停止位
     _1_5:              - 1.5位停止位
     _2:                - 2位停止位
   |</PRE>}
  TByteSize = (_5, _6, _7, _8);
  {* 串口通讯可用数据位数
   |<PRE>
     _5:                - 5位数据
     _6:                - 6位数据
     _7:                - 7位数据
     _8:                - 8位数据
   |</PRE>}
  TDtrControl = (DtrEnable, DtrDisable, DtrHandshake);
  {* 串口通讯中使用DTR(数据终端就绪)信号进行流量控制的方式
   |<PRE>
     DtrEnable:         - 允许DTR线并保持
     DtrDisable:        - 禁止DTR线并保持
     DtrHandshake:      - 允许DTR握手
   |</PRE>}
  TRtsControl = (RtsEnable, RtsDisable, RtsHandshake, RtsTransmissionAvailable);
  {* 串口通讯中使用RTS(请求发送)信号进行流量控制的方式
   |<PRE>
     RtsEnable:         - 允许RTS并保持
     RtsDisable:        - 禁止RTS并保持
     RtsHandshake:      - 允许RTS握手
     RtsTransmissionAvailable: - 使用触发方式
   |</PRE>}

  ERS232Error = class(Exception);
  EInvalidXonXoffChar = class(Exception);

//------------------------------------------------------------------------------
// RS232串口通讯设置类
//------------------------------------------------------------------------------

{ TCnRS232Config }

  TCnRS232Config = class(TCnPersistent)
  {* RS232串口通讯设置持久性类}
  private
    FXoffChar: Char;
    FReplacedChar: Char;
    FXonChar: Char;
    FOutx_CtsFlow: Boolean;
    FOutx_DsrFlow: Boolean;
    FParityCheck: Boolean;
    FIgnoreNullChar: Boolean;
    FInx_XonXoffFlow: Boolean;
    FTxContinueOnXoff: Boolean;
    FReplaceWhenParityError: Boolean;
    FOutx_XonXoffFlow: Boolean;
    FDsrSensitivity: Boolean;
    FBaudRate: DWord;
    FByteSize: TByteSize;
    FDtrControl: TDtrControl;
    FParity: TParity;
    FRtsControl: TRtsControl;
    FStopBits: TStopBits;
    FXoffLimit: WORD;
    FXonLimit: WORD;
    procedure SetBaudRate(const Value: DWord);
    procedure SetByteSize(const Value: TByteSize);
    procedure SetDsrSensitivity(const Value: Boolean);
    procedure SetDtrControl(const Value: TDtrControl);
    procedure SetIgnoreNullChar(const Value: Boolean);
    procedure SetInx_XonXoffFlow(const Value: Boolean);
    procedure SetOutx_CtsFlow(const Value: Boolean);
    procedure SetOutx_DsrFlow(const Value: Boolean);
    procedure SetOutx_XonXoffFlow(const Value: Boolean);
    procedure SetParityCheck(const Value: Boolean);
    procedure SetReplacedChar(const Value: Char);
    procedure SetReplaceWhenParityError(const Value: Boolean);
    procedure SetRtsControl(const Value: TRtsControl);
    procedure SetStopBits(const Value: TStopBits);
    procedure SetTxContinueOnXoff(const Value: Boolean);
    procedure SetXoffChar(const Value: Char);
    procedure SetXoffLimit(const Value: WORD);
    procedure SetXonChar(const Value: Char);
    procedure SetXonLimit(const Value: WORD);
  public
    constructor Create; override;
    {* 类构造器，创建类实例}
    procedure Assign(Source: TPersistent); override;
    {* 在两个对象之间赋值}
    procedure GetDCB(var DCB: TDCB);
    {* 从当前设置中取DCB结构}
    procedure SetDCB(const DCB: TDCB);
    {* 根据DCB结构进行设置}
  published
    property BaudRate: DWord read FBaudRate write SetBaudRate default 9600;
    {* 串口通讯波特率}
    property ParityCheck: Boolean read FParityCheck write SetParityCheck default False;
    {* 设置是否允许奇偶校验}
    property Outx_CtsFlow: Boolean read FOutx_CtsFlow write SetOutx_CtsFlow default False;
    {* 设置是否使用CTS(清除发送)信号进行输出流量控制}
    property Outx_DsrFlow: Boolean read FOutx_DsrFlow write SetOutx_DsrFlow default False;
    {* 设置是否使用DSR(数据设备就绪)信号进行输出流量控制}
    property DtrControl: TDtrControl read FDtrControl write SetDtrControl default DtrEnable;
    {* 使用DTR(数据终端就绪)信号进行流量控制的方式}
    property DsrSensitivity: Boolean read FDsrSensitivity write SetDsrSensitivity default False;
    {* 指定通信驱动程序对DSR信号的状态是否敏感。
     |<BR> 如果为真，当Modem的DSR输入线为低时，驱动程序将忽略接收到的任何字节。}
    property TxContinueOnXoff: Boolean read FTxContinueOnXoff write SetTxContinueOnXoff default False;
    {* 指定当接收缓冲区已满，已发送“Xoff字符”后发送是否停止。
     |<BR> 如果为真，当被填满的接收缓冲区中的字节数未达到“Xoff阈值”并且驱动
       程序发送了“Xoff字符”后停止接收字节时，继续发送；
     |<BR> 如果为假，当被排空的缓冲区中的字节数不足“Xon阈值”个字节，
       且驱动程序发送了“Xon字符”后恢复接收时，继续发送。}
    property Outx_XonXoffFlow: Boolean read FOutx_XonXoffFlow write SetOutx_XonXoffFlow default False;
    {* 指定数据发送时是否使用Xon/Xoff信息流控制
     |<BR> 如果为真，当接收到“Xoff字符”时暂停发送，并在接收到“Xon字符”时恢复发送。}
    property Inx_XonXoffFlow: Boolean read FInx_XonXoffFlow write SetInx_XonXoffFlow default False;
    {* 指定数据接收时是否使用Xon/Xoff信息流控制
     |<BR> 如果为真，当接收缓冲区快满，只剩“Xoff阈值”个字符空闲时发送“Xoff字符”；
       当接收缓冲区中只有“Xon阈值”个字符时，发送“Xon字符”。}
    property ReplaceWhenParityError: Boolean read FReplaceWhenParityError write SetReplaceWhenParityError default False;
    {* 指定出现奇偶校验错时是否用指定字符ReplacedChar代替}
    property IgnoreNullChar: Boolean read FIgnoreNullChar write SetIgnoreNullChar default False;
    {* 指定是否丢弃接收到的NULL(ASCII 0)字符}
    property RtsControl: TRtsControl read FRtsControl write SetRtsControl default RtsEnable;
    {* 指定使用RTS(请求发送)信号进行流量控制的方式}
    property XonLimit: WORD read FXonLimit write SetXonLimit default 500;
    {* 指明在发送“Xon字符”之前，接收缓冲区中允许的最少字符数。}
    property XoffLimit: WORD read FXoffLimit write SetXoffLimit default 500;
    {* 指明在发送“Xoff字符”之前，接收缓冲区中允许的最多字符数。
     |<BR> 接收缓冲区的长度减去该值，即允许的最多字符数。}
    property ByteSize: TByteSize read FByteSize write SetByteSize default _8;
    {* 可用数据位数}
    property Parity: TParity read FParity write FParity default paNone;
    {* 奇偶校验方式}
    property StopBits: TStopBits read FStopBits write SetStopBits default _1;
    {* 停止位数}
    property XonChar: Char read FXonChar write SetXonChar default chr($11);
    {* 发送和接收的“Xon字符”的ASCII码，表示允许继续传输。
     |<BR> 其值不能与XoffChar相同。}
    property XoffChar: Char read FXoffChar write SetXoffChar default chr($13);
    {* 发送和接收的“Xoff字符”的ASCII码，表示允许暂停传输。
     |<BR> 其值不能与XonChar相同。}
    property ReplacedChar: Char read FReplacedChar write SetReplacedChar default chr(0);
    {* 指定出现奇偶校验错时用来替换的字符的ASCII码，见ReplaceWhenParityError}
  end;

//------------------------------------------------------------------------------
// RS232串口通讯设置超时类
//------------------------------------------------------------------------------

{ TCnRS232Timeouts }

  TCnRS232Timeouts = class(TCnPersistent)
  {* RS232串口通讯超时设置持久性类}
  private
    FReadTotalTimeoutConstant: DWord;
    FReadIntervalTimeout: DWord;
    FReadTotalTimeoutMultiplier: DWord;
    FWriteTotalTimeoutConstant: DWord;
    FWriteTotalTimeoutMultiplier: DWord;
    procedure SetReadIntervalTimeout(const Value: DWord);
    procedure SetReadTotalTimeoutConstant(const Value: DWord);
    procedure SetReadTotalTimeoutMultiplier(const Value: DWord);
    procedure SetWriteTotalTimeoutConstant(const Value: DWord);
    procedure SetWriteTotalTimeoutMultiplier(const Value: DWord);
  public
    constructor Create; override;
    {* 类构造器，创建类实例}
    procedure Assign(Source: TPersistent); override;
    {* 在两个对象之间赋值}
    function GetCommTimeouts: TCommTimeouts;
    {* 从当前设置中取TCommTimeouts结构}
    procedure SetCommTimeouts(const Value: TCommTimeouts);
    {* 根据TCommTimeouts结构进行设置}
  published
    property ReadIntervalTimeout: DWord read FReadIntervalTimeout write SetReadIntervalTimeout default 10;
    {* 指定通信线路上两个字符到达之间的最大时间。
     |<BR> 在读取操作期间，从接收到第一个字符时开始计时，若任意两个字符到达之
       间的时间间隔超过这个最大值，则读取操作完成，返回缓冲数据。
     |<BR> 如果置0，表示不使用间隔超时。}
    property ReadTotalTimeoutMultiplier: DWord read FReadTotalTimeoutMultiplier write SetReadTotalTimeoutMultiplier default 0;
    {* 用于设定读总超时时间的系数。
     |<BR> 读总超时时间 = (总超时系数 X 接收字符数) + 总超时常量
     |<BR> 常量和系数可分别为0。如果均为0，则不使用总超时设定。}
    property ReadTotalTimeoutConstant: DWord read FReadTotalTimeoutConstant write SetReadTotalTimeoutConstant default 0;
    {* 用于设定读总超时时间的常量值。
     |<BR> 读总超时时间 = (总超时系数 X 接收字符数) + 总超时常量
     |<BR> 常量和系数可分别为0。如果均为0，则不使用总超时设定。}
    property WriteTotalTimeoutMultiplier: DWord read FWriteTotalTimeoutMultiplier write SetWriteTotalTimeoutMultiplier default 0;
    {* 用于设定写总超时时间的系数。
     |<BR> 写总超时时间 = (总超时系数 X 接收字符数) + 总超时常量
     |<BR> 常量和系数可分别为0。如果均为0，则不使用总超时设定。}
    property WriteTotalTimeoutConstant: DWord read FWriteTotalTimeoutConstant write SetWriteTotalTimeoutConstant default 0;
    {* 用于设定写总超时时间的常量值。
     |<BR> 写总超时时间 = (总超时系数 X 接收字符数) + 总超时常量
     |<BR> 常量和系数可分别为0。如果均为0，则不使用总超时设定。}
  end;

  TReceiveDataEvent = procedure(Sender: TObject; Buffer: Pointer;
    BufferLength: WORD) of object;
  {* 串口通讯中接收到数据事件。
   |<PRE>
     Buffer: Pointer    指向该数据缓冲区
     BufferLength: WORD 数据长度
   |</PRE>}
  TModemStateChangeEvent = procedure(Sender: TObject; ModemEvent: DWord) of object;
  {* 串口通讯中Modem状态变更事件。
   |<PRE>
     参数ModemEvent可取以下值：
     ME_CTS = 1
     ME_DSR = 2
     ME_RING = 4
     ME_RLSD = 8
   |</PRE>}
  TReceiveErrorEvent = procedure(Sender: TObject; EventMask: DWord) of object;
  {* 串口通讯错误事件。}
  TSendDataEmptyEvent = procedure(Sender: TObject) of object;
  {* 串口通讯数据缓冲区空事件，该事件表明缓冲区数据已成功发送完成。}

//------------------------------------------------------------------------------
// RS232串口通讯读线程
//------------------------------------------------------------------------------

{ TReadThread }

  TReadThread = class(TThread)
  protected
    procedure Execute; override;
  public
    hCommFile: THandle;
    hCloseEvent: THandle;
    hComm32Window: THandle;

    function SetupCommEvent(lpOverlappedCommEvent: POverlapped;
      var lpfdwEvtMask: DWord): Boolean;
    function SetupReadEvent(lpOverlappedRead: POverlapped;
      lpszInputBuffer: LPSTR; dwSizeofBuffer: DWord;
      var lpnNumberOfBytesRead: DWord): Boolean;
    function HandleCommEvent(lpOverlappedCommEvent: POverlapped;
      var lpfdwEvtMask: DWord; fRetrieveEvent: Boolean): Boolean;
    function HandleReadEvent(lpOverlappedRead: POverlapped;
      lpszInputBuffer: LPSTR; dwSizeofBuffer: DWord;
      var lpnNumberOfBytesRead: DWord): Boolean;
    function HandleReadData(lpszInputBuffer: LPCSTR; dwSizeofBuffer: DWord): Boolean;
    function ReceiveData(lpNewString: LPSTR; dwSizeofNewString: DWord): Bool;
    function ReceiveError(EvtMask: DWord): Bool;
    function ModemStateChange(ModemEvent: DWord): Bool;
    procedure PostHangupCall;
  end;

//------------------------------------------------------------------------------
// RS232串口通讯写线程
//------------------------------------------------------------------------------

{ TWriteThread }

  TWriteThread = class(TThread)
  protected
    procedure Execute; override;
    function HandleWriteData(lpOverlappedWrite: POverlapped;
      pDataToWrite: PAnsiChar; dwNumberOfBytesToWrite: DWord): Boolean;
  public
    hCommFile: THandle;
    hCloseEvent: THandle;
    hComm32Window: THandle;
    pFSendDataEmpty: ^Boolean;
    procedure PostHangupCall;
  end;

//------------------------------------------------------------------------------
// RS232串口通讯组件
//------------------------------------------------------------------------------

{ TCnRS232 }

  TCnRS232 = class(TCnComponent)
  {* RS232串口通讯组件。
   |<PRE>
     * 组件采用单独的读写线程以overlapped方式进行串口通讯。
     * 使用时先要通过 StartComm 方法打开串口，通讯完成可使用 StopComm 关闭。
     * 当串口接收到数据时，会产生 OnReceiveData 事件，传递数据缓冲区指针和数据长度。
     * 向串口写数据使用 WriteCommData 方法完成，该方法调用后，组件会产生一个写线程
       在后台发送数据，发送完毕产生 OnSendDataEmpty 事件。
     * 该组件可搭配串口设置对话框组件 TRS232Dialog 使用。
     * 注：Timeouts 超时设置中的 ReadIntervalTimeout 决定了接收数据时对数据分块的
       方法，如果不能接收到预期长度的数据，请尝试调整该属性。
   |</PRE>}
  private
    { Private declarations }
    ReadThread: TReadThread;
    WriteThread: TWriteThread;
    hCommFile: THandle;
    hCloseEvent: THandle;
    FHWnd: THandle;
    FSendDataEmpty: Boolean;
    FCommName: string;
    FCommConfig: TCnRS232Config;
    FTimeouts: TCnRS232Timeouts;
    FOnRequestHangup: TNotifyEvent;
    FOnReceiveData: TReceiveDataEvent;
    FOnReceiveError: TReceiveErrorEvent;
    FOnSendDataEmpty: TSendDataEmptyEvent;
    FOnModemStateChange: TModemStateChangeEvent;
    procedure CommWndProc(var Msg: TMessage);
    procedure _SetCommState;
    procedure _SetCommTimeout;
    procedure SetCommConfig(const Value: TCnRS232Config);
    procedure SetTimeouts(const Value: TCnRS232Timeouts);
    function GetConnected: Boolean;
  protected
    { Protected declarations }
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    procedure ConfigChanged(Sender: TObject);
    procedure TimeoutsChanged(Sender: TObject);
    procedure CloseReadThread;
    procedure CloseWriteThread;
    procedure ReceiveData(Buffer: PAnsiChar; BufferLength: WORD); virtual;
    procedure ReceiveError(EvtMask: DWord); virtual;
    procedure ModemStateChange(ModemEvent: DWord); virtual;
    procedure RequestHangup; virtual;
    procedure _SendDataEmpty; virtual;
    property OnModemStateChange: TModemStateChangeEvent read FOnModemStateChange write FOnModemStateChange;
  public
    { Public declarations }
    property Handle: THandle read hCommFile;
    {* 串口设备句柄}
    property SendDataEmpty: Boolean read FSendDataEmpty;
    {* 当前发送数据缓冲区是否为空，运行期只读属性}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方式}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartComm;
    {* 根据当前设置打开串口设备}
    procedure StopComm;
    {* 关闭已打开的串口设备}
    procedure ReadFromIni(Ini: TCustomIniFile; const Section: string); overload; virtual;
    {* 从INI配置对象中读取串口通讯设置，包含了组件的所有通讯参数，可重载
     |<BR> 技巧：可以使用TRegIniFile对象来将设置保存到注册表中}
    procedure WriteToIni(Ini: TCustomIniFile; const Section: string); overload; virtual;
    {* 将当前的串口通讯设置保存到INI配置对象，包含了组件的所有通讯参数，可重载
     |<BR> 技巧：可以使用TRegIniFile对象来从注册表中读取}
    procedure ReadFromIni(const FileName: string; const Section: string); overload;
    {* 从INI文件中读取串口通讯设置，包含了组件的所有通讯参数}
    procedure WriteToIni(const FileName: string; const Section: string); overload;
    {* 将当前的串口通讯设置保存到INI文件中，包含了组件的所有通讯参数}
    function WriteCommData(pDataToWrite: PAnsiChar; dwSizeofDataToWrite: WORD): Boolean;
    {* 向串口写数据方法
     |<PRE>
       pDataToWrite: PAnsiChar        - 要发送的数据缓冲区指针
       dwSizeofDataToWrite: WORD  - 数据块的长度
     |</PRE>}
    function GetModemState: DWord;
    {* 取当前Modem状态}
    property Connected: Boolean read GetConnected;
    {* 标识当前端口是否已打开 }
  published
    { Published declarations }
    property CommName: string read FCommName write FCommName;
    {* 串口端口名，为类似 COM1、COM2 这样的字符串。
     |<BR> 如果指定错误的设备名，打开设备时将产生错误。}
    property CommConfig: TCnRS232Config read FCommConfig write SetCommConfig;
    {* 串口通讯设置}
    property Timeouts: TCnRS232Timeouts read FTimeouts write SetTimeouts;
    {* 串口通讯超时设置}
    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    {* 接收到数据事件}
    property OnReceiveError: TReceiveErrorEvent read FOnReceiveError write FOnReceiveError;
    {* 接收数据错误事件}
    property OnRequestHangup: TNotifyEvent read FOnRequestHangup write FOnRequestHangup;
    {* 接收中断通讯事件}
    property OnSendDataEmpty: TSendDataEmptyEvent read FOnSendDataEmpty write FOnSendDataEmpty;
    {* 数据发送缓冲区空事件}
  end;

implementation

const
  INPUT_BUFFER_SIZE = 2048;

{ TReadThread }

procedure TReadThread.Execute;
var
  szInputBuffer: array[0..INPUT_BUFFER_SIZE - 1] of AnsiChar;
  nNumberOfBytesRead: DWord;

  HandlesToWaitFor: array[0..2] of THandle;
  dwHandleSignaled: DWord;

  fdwEvtMask: DWord;

  // Needed for overlapped I/O (ReadFile)
  overlappedRead: TOverlapped;

  // Needed for overlapped Comm Event handling.
  overlappedCommEvent: TOverlapped;
label
  EndReadThread;
begin
  FillChar(overlappedRead, SizeOf(overlappedRead), 0);
  FillChar(overlappedCommEvent, SizeOf(overlappedCommEvent), 0);

  // Lets put an event in the Read overlapped structure.
  overlappedRead.hEvent := CreateEvent(nil, True, True, nil);
  if overlappedRead.hEvent = 0 then
  begin
    PostHangupCall;
    goto EndReadThread;
  end;

  // And an event for the CommEvent overlapped structure.
  overlappedCommEvent.hEvent := CreateEvent(nil, True, True, nil);
  if overlappedCommEvent.hEvent = 0 then
  begin
    PostHangupCall;
    goto EndReadThread;
  end;

  // We will be waiting on these objects.
  HandlesToWaitFor[0] := hCloseEvent;
  HandlesToWaitFor[1] := overlappedCommEvent.hEvent;
  HandlesToWaitFor[2] := overlappedRead.hEvent;

  // Setup CommEvent handling.

  // Set the comm mask so we receive error signals.
  if not SetCommMask(hCommFile, EV_ERR or EV_RLSD or EV_RING) then
  begin
    PostHangupCall;
    goto EndReadThread;
  end;

  // Start waiting for CommEvents (Errors)
  if not SetupCommEvent(@overlappedCommEvent, fdwEvtMask) then
    goto EndReadThread;

  // Start waiting for Read events.
  if not SetupReadEvent(@overlappedRead,
    PAnsiChar(@szInputBuffer[0]), INPUT_BUFFER_SIZE,
    nNumberOfBytesRead) then
    goto EndReadThread;

  // Keep looping until we break out.
  while True do
  begin
    // Wait until some event occurs (data to read; error; stopping).
    dwHandleSignaled := WaitForMultipleObjects(3, @HandlesToWaitFor,
      False, INFINITE);

    // Which event occured?
    case dwHandleSignaled of
      WAIT_OBJECT_0:                    // Signal to end the thread.
        begin
          // Time to exit.
          goto EndReadThread;
        end;

      WAIT_OBJECT_0 + 1:                // CommEvent signaled.
        begin
          // Handle the CommEvent.
          if not HandleCommEvent(@overlappedCommEvent, fdwEvtMask, True) then
            goto EndReadThread;

          // Start waiting for the next CommEvent.
          if not SetupCommEvent(@overlappedCommEvent, fdwEvtMask) then
            goto EndReadThread;
              {break;??}
        end;

      WAIT_OBJECT_0 + 2:                // Read Event signaled.
        begin
          // Get the new data!
          if not HandleReadEvent(@overlappedRead,
            PAnsiChar(@szInputBuffer[0]),
            INPUT_BUFFER_SIZE,
            nNumberOfBytesRead) then
            goto EndReadThread;

          // Wait for more new data.
          if not SetupReadEvent(@overlappedRead,
            PAnsiChar(@szInputBuffer[0]), INPUT_BUFFER_SIZE,
            nNumberOfBytesRead) then
            goto EndReadThread;
              {break;}
        end;

      WAIT_FAILED:                      // Wait failed.  Shouldn't happen.
        begin
          PostHangupCall;
          goto EndReadThread;
        end
    else                                // This case should never occur.
      begin
        PostHangupCall;
        goto EndReadThread;
      end
    end                                 {case dwHandleSignaled}
  end;                                  {while True}

  // Time to clean up Read Thread.
EndReadThread:

  PurgeComm(hCommFile, PURGE_RXABORT + PURGE_RXCLEAR);
  CloseHandle(overlappedRead.hEvent);
  CloseHandle(overlappedCommEvent.hEvent)
end;                                    {TReadThread.Execute}

function TReadThread.SetupReadEvent(lpOverlappedRead: POverlapped;
  lpszInputBuffer: LPSTR; dwSizeofBuffer: DWord;
  var lpnNumberOfBytesRead: DWord): Boolean;
var
  dwLastError: DWord;
label
  StartSetupReadEvent;
begin
  Result := False;

StartSetupReadEvent:

  // Make sure the CloseEvent hasn't been signaled yet.
  // Check is needed because this function is potentially recursive.
  if WAIT_TIMEOUT <> WaitForSingleObject(hCloseEvent, 0) then
    Exit;

  // Start the overlapped ReadFile.
  if ReadFile(hCommFile,
    lpszInputBuffer^, dwSizeofBuffer,
    lpnNumberOfBytesRead, lpOverlappedRead) then
  begin
    // This would only happen if there was data waiting to be read.

    // Handle the data.
    if lpnNumberOfBytesRead > 0 then // If got zero, do not handle and continue
      if not HandleReadData(lpszInputBuffer, lpnNumberOfBytesRead) then
        Exit;

    // Start waiting for more data.
    goto StartSetupReadEvent
  end;

  // ReadFile failed.  Expected because of overlapped I/O.
  dwLastError := GetLastError;

  // LastError was ERROR_IO_PENDING, as expected.
  if dwLastError = ERROR_IO_PENDING then
  begin
    Result := True;
    Exit;
  end;

  // Its possible for this error to occur if the
  // service provider has closed the port.  Time to end.
  if dwLastError = ERROR_INVALID_HANDLE then
    Exit;

  // Unexpected error come here. No idea what could cause this to happen.
  PostHangupCall;
end;                                    {TReadThread.SetupReadEvent}

function TReadThread.HandleReadData(lpszInputBuffer: LPCSTR; dwSizeofBuffer: DWord): Boolean;
var
  lpszPostedBytes: LPSTR;
begin
  Result := False;

  // If we got data and didn't just time out empty...
  if dwSizeofBuffer <> 0 then
  begin
    // Do something with the bytes read.
    lpszPostedBytes := PAnsiChar(LocalAlloc(LPTR, dwSizeofBuffer + 1));
    if lpszPostedBytes = nil {NULL} then
    begin
      // Out of memory
      PostHangupCall;
      Exit;
    end;

    Move(lpszInputBuffer^, lpszPostedBytes^, dwSizeofBuffer);
    lpszPostedBytes[dwSizeofBuffer] := #0;

    Result := ReceiveData(lpszPostedBytes, dwSizeofBuffer)
  end;
end;                                    {TReadThread.HandleReadData}

function TReadThread.HandleReadEvent(lpOverlappedRead: POverlapped;
  lpszInputBuffer: LPSTR; dwSizeofBuffer: DWord;
  var lpnNumberOfBytesRead: DWord): Boolean;
var
  dwLastError: DWord;
begin
  Result := False;

  if GetOverlappedResult(hCommFile,
    lpOverlappedRead^, lpnNumberOfBytesRead, False) then
  begin
    Result := HandleReadData(lpszInputBuffer, lpnNumberOfBytesRead);
    Exit
  end;

  // Error in GetOverlappedResult; handle it.

  dwLastError := GetLastError;

  // Its possible for this error to occur if the
  // service provider has closed the port.  Time to end.
  if dwLastError = ERROR_INVALID_HANDLE then
    Exit;

  // Unexpected error come here. No idea what could cause this to happen.
  PostHangupCall;
end;                                    {TReadThread.HandleReadEvent}

function TReadThread.SetupCommEvent(lpOverlappedCommEvent: POverlapped;
  var lpfdwEvtMask: DWord): Boolean;
var
  dwLastError: DWord;
label
  StartSetupCommEvent;
begin
  Result := False;

StartSetupCommEvent:

  // Make sure the CloseEvent hasn't been signaled yet.
  // Check is needed because this function is potentially recursive.
  if WAIT_TIMEOUT <> WaitForSingleObject(hCloseEvent, 0) then
    Exit;

  // Start waiting for Comm Errors.
  if WaitCommEvent(hCommFile, lpfdwEvtMask, lpOverlappedCommEvent) then
  begin
    // This could happen if there was an error waiting on the
    // comm port.  Lets try and handle it.

    if not HandleCommEvent(nil, lpfdwEvtMask, False) then
    begin
      {??? GetOverlappedResult does not handle "NIL" as defined by Borland}
      Exit
    end;

    // What could cause infinite recursion at this point?
    goto StartSetupCommEvent;
  end;

  // We expect ERROR_IO_PENDING returned from WaitCommEvent
  // because we are waiting with an overlapped structure.

  dwLastError := GetLastError;

  // LastError was ERROR_IO_PENDING, as expected.
  if dwLastError = ERROR_IO_PENDING then
  begin
    Result := True;
    Exit;
  end;

  // Its possible for this error to occur if the
  // service provider has closed the port.  Time to end.
  if dwLastError = ERROR_INVALID_HANDLE then
    Exit;

  // Unexpected error. No idea what could cause this to happen.
  PostHangupCall;
end;                                    {TReadThread.SetupCommEvent}

function TReadThread.HandleCommEvent(lpOverlappedCommEvent: POverlapped;
  var lpfdwEvtMask: DWord; fRetrieveEvent: Boolean): Boolean;
var
  dwDummy: DWord;
  dwErrors: DWord;
  dwLastError: DWord;
  dwModemEvent: DWord;
begin
  Result := False;

  // If this fails, it could be because the file was closed (and I/O is
  // finished) or because the overlapped I/O is still in progress.  In
  // either case (or any others) its a bug and return FALSE.
  if fRetrieveEvent then
  begin
    if not GetOverlappedResult(hCommFile,
      lpOverlappedCommEvent^, dwDummy, False) then
    begin
      dwLastError := GetLastError;

      // Its possible for this error to occur if the
      // service provider has closed the port.  Time to end.
      if dwLastError = ERROR_INVALID_HANDLE then
        Exit;

      PostHangupCall;
      Exit;
    end
  end;

  // Was the event an error?
  if (lpfdwEvtMask and EV_ERR) <> 0 then
  begin
    // Which error was it?
    if not ClearCommError(hCommFile, dwErrors, nil) then
    begin
      dwLastError := GetLastError;

      // Its possible for this error to occur if the
      // service provider has closed the port.  Time to end.
      if dwLastError = ERROR_INVALID_HANDLE then
        Exit;

      PostHangupCall;
      Exit;
    end;

    // Its possible that multiple errors occured and were handled
    // in the last ClearCommError.  Because all errors were signaled
    // individually, but cleared all at once, pending comm events
    // can yield EV_ERR while dwErrors equals 0.  Ignore this event.

    if not ReceiveError(dwErrors) then
      Exit;

    Result := True;
  end;

  dwModemEvent := 0;

  if ((lpfdwEvtMask and EV_RLSD) <> 0) then
    dwModemEvent := ME_RLSD;
  if ((lpfdwEvtMask and EV_RING) <> 0) then
    dwModemEvent := dwModemEvent or ME_RING;

  if dwModemEvent <> 0 then
  begin
    if not ModemStateChange(dwModemEvent) then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;

  if ((lpfdwEvtMask and EV_ERR) = 0) and (dwModemEvent = 0) then
  begin
    // Should not have gotten here.
    PostHangupCall;
  end
end;                                    {TReadThread.HandleCommEvent}

function TReadThread.ReceiveData(lpNewString: LPSTR; dwSizeofNewString: DWord): Bool;
begin
  Result := False;

  if not PostMessage(hComm32Window, PWM_GOTCOMMDATA,
    WPARAM(dwSizeofNewString), LPARAM(lpNewString)) then
    PostHangupCall
  else
    Result := True;
end;

function TReadThread.ReceiveError(EvtMask: DWord): Bool;
begin
  Result := False;

  if not PostMessage(hComm32Window, PWM_RECEIVEERROR, 0, LPARAM(EvtMask)) then
    PostHangupCall
  else
    Result := True;
end;

function TReadThread.ModemStateChange(ModemEvent: DWord): Bool;
begin
  Result := False;

  if not PostMessage(hComm32Window, PWM_MODEMSTATECHANGE, 0, LPARAM(ModemEvent)) then
    PostHangupCall
  else
    Result := True;
end;

procedure TReadThread.PostHangupCall;
begin
  PostMessage(hComm32Window, PWM_REQUESTHANGUP, 0, 0)
end;

{ TWriteThread }

procedure TWriteThread.Execute;
var
  Msg: TMsg;
  dwHandleSignaled: DWord;
  overlappedWrite: TOverlapped;
  CompleteOneWriteRequire: Boolean;
label
  EndWriteThread;
begin
  // Needed for overlapped I/O.
  FillChar(overlappedWrite, SizeOf(overlappedWrite), 0); {0, 0, 0, 0, NULL}

  overlappedWrite.hEvent := CreateEvent(nil, True, True, nil);
  if overlappedWrite.hEvent = 0 then
  begin
    PostHangupCall;
    goto EndWriteThread;
  end;

  CompleteOneWriteRequire := True;

  // This is the main loop.  Loop until we break out.
  while True do
  begin
    if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      // If there are no messages pending, wait for a message or
      // the CloseEvent.

      pFSendDataEmpty^ := True;

      if CompleteOneWriteRequire then
      begin
        if not PostMessage(hComm32Window, PWM_SENDDATAEMPTY, 0, 0) then
        begin
          PostHangupCall;
          goto EndWriteThread;
        end
      end;

      CompleteOneWriteRequire := False;

      dwHandleSignaled := MsgWaitForMultipleObjects(1, hCloseEvent, False,
        INFINITE, QS_ALLINPUT);

      case dwHandleSignaled of
        WAIT_OBJECT_0:                  // CloseEvent signaled!
          begin
            // Time to exit.
            goto EndWriteThread;
          end;

        WAIT_OBJECT_0 + 1:              // New message was received.
          begin
            // Get the message that woke us up by looping again.
            Continue
          end;

        WAIT_FAILED:                    // Wait failed.  Shouldn't happen.
          begin
            PostHangupCall;
            goto EndWriteThread;
          end

      else                              // This case should never occur.
        begin
          PostHangupCall;
          goto EndWriteThread;
        end
      end
    end;

    // Make sure the CloseEvent isn't signaled while retrieving messages.
    if WAIT_TIMEOUT <> WaitForSingleObject(hCloseEvent, 0) then
      goto EndWriteThread;

    // Process the message.
    // This could happen if a dialog is created on this thread.
    // This doesn't occur in this sample, but might if modified.
    if Msg.HWND <> 0 {NULL} then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
      Continue;
    end;

    // Handle the message.
    case Msg.message of
      PWM_COMMWRITE:                    // New string to write to Comm port.
        begin
          // Write the string to the comm port.  HandleWriteData
          // does not return until the whole string has been written,
          // an error occurs or until the CloseEvent is signaled.
          if not HandleWriteData(@overlappedWrite,
            PAnsiChar(Msg.LPARAM), DWord(Msg.WPARAM)) then
          begin
            // If it failed, either we got a signal to end or there
            // really was a failure.

            LocalFree(HLOCAL(Msg.LPARAM));
            goto EndWriteThread
          end;

          CompleteOneWriteRequire := True;
          // Data was sent in a LocalAlloc()d buffer.  Must free it.
          LocalFree(HLOCAL(Msg.LPARAM))
        end
    end
  end;                                  {main loop}

  // Thats the end.  Now clean up.
EndWriteThread:

  PurgeComm(hCommFile, PURGE_TXABORT + PURGE_TXCLEAR);
  pFSendDataEmpty^ := True;
  CloseHandle(overlappedWrite.hEvent);
end;                                    {TWriteThread.Execute}

function TWriteThread.HandleWriteData(lpOverlappedWrite: POverlapped;
  pDataToWrite: PAnsiChar; dwNumberOfBytesToWrite: DWord): Boolean;
var
  dwLastError,
  dwNumberOfBytesWritten,
  dwWhereToStartWriting,
  dwHandleSignaled: DWord;
  HandlesToWaitFor: array[0..1] of THandle;
begin
  Result := False;

  dwNumberOfBytesWritten := 0;
  dwWhereToStartWriting := 0;           // Start at the beginning.

  HandlesToWaitFor[0] := hCloseEvent;
  HandlesToWaitFor[1] := lpOverlappedWrite^.hEvent;

  // Keep looping until all characters have been written.
  repeat
    // Start the overlapped I/O.
    if not WriteFile(hCommFile,
      pDataToWrite[dwWhereToStartWriting],
      dwNumberOfBytesToWrite, dwNumberOfBytesWritten,
      lpOverlappedWrite) then
    begin
      // WriteFile failed.  Expected; lets handle it.
      dwLastError := GetLastError;

      // Its possible for this error to occur if the
      // service provider has closed the port.  Time to end.
      if dwLastError = ERROR_INVALID_HANDLE then
        Exit;

      // Unexpected error.  No idea what.
      if dwLastError <> ERROR_IO_PENDING then
      begin
        PostHangupCall;
        Exit;
      end;

      // This is the expected ERROR_IO_PENDING case.

      // Wait for either overlapped I/O completion,
      // or for the CloseEvent to get signaled.
      dwHandleSignaled := WaitForMultipleObjects(2, @HandlesToWaitFor,
        False, INFINITE);

      case dwHandleSignaled of
        WAIT_OBJECT_0:                  // CloseEvent signaled!
          begin
            // Time to exit.
            Exit
          end;

        WAIT_OBJECT_0 + 1:              // Wait finished.
          begin
            // Time to get the results of the WriteFile
            if not GetOverlappedResult(hCommFile,
              lpOverlappedWrite^,
              dwNumberOfBytesWritten, True) then
            begin
              dwLastError := GetLastError;

              // Its possible for this error to occur if the
              // service provider has closed the port.
              if dwLastError = ERROR_INVALID_HANDLE then
                Exit;

              // No idea what could cause another error.
              PostHangupCall;
              Exit
            end
          end;

        WAIT_FAILED:                    // Wait failed.  Shouldn't happen.
          begin
            PostHangupCall;
            Exit;
          end

      else                              // This case should never occur.
        begin
          PostHangupCall;
          Exit;
        end
      end                               {case}
    end;                                {WriteFile failure}

    // Some data was written.  Make sure it all got written.

    Dec(dwNumberOfBytesToWrite, dwNumberOfBytesWritten);
    Inc(dwWhereToStartWriting, dwNumberOfBytesWritten)
  until (dwNumberOfBytesToWrite <= 0);  // Write the whole thing!

  // Wrote the whole string.
  Result := True;
end;                                    {TWriteThread.HandleWriteData}

procedure TWriteThread.PostHangupCall;
begin
  PostMessage(hComm32Window, PWM_REQUESTHANGUP, 0, 0)
end;

{ TCnRS232Config }

procedure TCnRS232Config.Assign(Source: TPersistent);
begin
  if Source is TCnRS232Config then
  begin
    FXoffChar := TCnRS232Config(Source).FXoffChar;
    FReplacedChar := TCnRS232Config(Source).FReplacedChar;
    FXonChar := TCnRS232Config(Source).FXonChar;
    FOutx_CtsFlow := TCnRS232Config(Source).FOutx_CtsFlow;
    FOutx_DsrFlow := TCnRS232Config(Source).FOutx_DsrFlow;
    FParityCheck := TCnRS232Config(Source).FParityCheck;
    FIgnoreNullChar := TCnRS232Config(Source).FIgnoreNullChar;
    FInx_XonXoffFlow := TCnRS232Config(Source).FInx_XonXoffFlow;
    FTxContinueOnXoff := TCnRS232Config(Source).FTxContinueOnXoff;
    FReplaceWhenParityError := TCnRS232Config(Source).FReplaceWhenParityError;
    FOutx_XonXoffFlow := TCnRS232Config(Source).FOutx_XonXoffFlow;
    FDsrSensitivity := TCnRS232Config(Source).FDsrSensitivity;
    FBaudRate := TCnRS232Config(Source).FBaudRate;
    FByteSize := TCnRS232Config(Source).FByteSize;
    FDtrControl := TCnRS232Config(Source).FDtrControl;
    FParity := TCnRS232Config(Source).FParity;
    FRtsControl := TCnRS232Config(Source).FRtsControl;
    FStopBits := TCnRS232Config(Source).FStopBits;
    FXoffLimit := TCnRS232Config(Source).FXoffLimit;
    FXonLimit := TCnRS232Config(Source).FXonLimit;
    Changed;
  end
  else
    inherited;
end;

constructor TCnRS232Config.Create;
begin
  inherited Create;
  FBaudRate := 9600;
  FParityCheck := False;
  FOutx_CtsFlow := False;
  FOutx_DsrFlow := False;
  FDtrControl := DtrEnable;
  FDsrSensitivity := False;
  FTxContinueOnXoff := False;
  FOutx_XonXoffFlow := False;
  FInx_XonXoffFlow := False;
  FReplaceWhenParityError := False;
  FIgnoreNullChar := False;
  FRtsControl := RtsEnable;
  FXonLimit := 500;
  FXoffLimit := 500;
  FByteSize := _8;
  FParity := paNone;
  FStopBits := _1;
  FXonChar := chr($11);                 // Ctrl-Q
  FXoffChar := chr($13);                // Ctrl-S
  FReplacedChar := chr(0);
end;

procedure TCnRS232Config.GetDCB(var DCB: TDCB);
begin
  DCB.DCBlength := SizeOf(TDCB);
  DCB.BaudRate := FBaudRate;
  DCB.Flags := 1;
  if FParityCheck then
    DCB.Flags := DCB.Flags or 2;
  if FOutx_CtsFlow then
    DCB.Flags := DCB.Flags or 4;
  if FOutx_DsrFlow then
    DCB.Flags := DCB.Flags or 8;
  if FDtrControl = DtrEnable then
    DCB.Flags := DCB.Flags or $10
  else if FDtrControl = DtrHandshake then
    DCB.Flags := DCB.Flags or $20;
  if FDsrSensitivity then
    DCB.Flags := DCB.Flags or $40;
  if FTxContinueOnXoff then
    DCB.Flags := DCB.Flags or $80;
  if FOutx_XonXoffFlow then
    DCB.Flags := DCB.Flags or $100;
  if FInx_XonXoffFlow then
    DCB.Flags := DCB.Flags or $200;
  if FReplaceWhenParityError then
    DCB.Flags := DCB.Flags or $400;
  if FIgnoreNullChar then
    DCB.Flags := DCB.Flags or $800;
  if FRtsControl = RtsEnable then
    DCB.Flags := DCB.Flags or $1000
  else if FRtsControl = RtsHandshake then
    DCB.Flags := DCB.Flags or $2000
  else if FRtsControl = RtsTransmissionAvailable then
    DCB.Flags := DCB.Flags or $3000;
  DCB.XonLim := FXonLimit;
  DCB.XoffLim := FXoffLimit;
  DCB.ByteSize := Ord(FByteSize) + 5;
  DCB.Parity := Ord(FParity);
  DCB.StopBits := Ord(FStopBits);
  DCB.XonChar := AnsiChar(FXonChar);
  DCB.XoffChar := AnsiChar(FXoffChar);
  DCB.ErrorChar := AnsiChar(FReplacedChar);
end;

procedure TCnRS232Config.SetDCB(const DCB: TDCB);
begin
  FBaudRate := DCB.BaudRate;
  FParityCheck := DCB.Flags and 2 <> 0;
  FOutx_CtsFlow := DCB.Flags and 4 <> 0;
  FOutx_DsrFlow := DCB.Flags and 8 <> 0;
  if DCB.Flags and $10 <> 0 then
    FDtrControl := DtrEnable
  else if DCB.Flags and $20 <> 0 then
    FDtrControl := DtrHandshake
  else
    FDtrControl := DtrDisable;
  FDsrSensitivity := DCB.Flags and $40 <> 0;
  FTxContinueOnXoff := DCB.Flags and $80 <> 0;
  FOutx_XonXoffFlow := DCB.Flags and $100 <> 0;
  FInx_XonXoffFlow := DCB.Flags and $200 <> 0;
  FReplaceWhenParityError := DCB.Flags and $400 <> 0;
  FIgnoreNullChar := DCB.Flags and $800 <> 0;
  if DCB.Flags and $1000 <> 0 then
    FRtsControl := RtsEnable
  else if DCB.Flags and $2000 <> 0 then
    FRtsControl := RtsHandshake
  else if DCB.Flags and $3000 <> 0 then
    FRtsControl := RtsTransmissionAvailable
  else
    FRtsControl := RtsDisable;
  FXonLimit := DCB.XonLim;
  FXoffLimit := DCB.XoffLim;
  FByteSize := TByteSize(DCB.ByteSize - 5);
  FParity := TParity(DCB.Parity);
  FStopBits := TStopBits(DCB.StopBits);
  FXonChar := Char(DCB.XonChar);
  FXoffChar := Char(DCB.XoffChar);
  FReplacedChar := Char(DCB.ErrorChar);
end;

procedure TCnRS232Config.SetBaudRate(const Value: DWord);
begin
  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetByteSize(const Value: TByteSize);
begin
  if FByteSize <> Value then
  begin
    FByteSize := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetDsrSensitivity(const Value: Boolean);
begin
  if FDsrSensitivity <> Value then
  begin
    FDsrSensitivity := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetDtrControl(const Value: TDtrControl);
begin
  if FDtrControl <> Value then
  begin
    FDtrControl := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetIgnoreNullChar(const Value: Boolean);
begin
  if FIgnoreNullChar <> Value then
  begin
    FIgnoreNullChar := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetInx_XonXoffFlow(const Value: Boolean);
begin
  if FInx_XonXoffFlow <> Value then
  begin
    FInx_XonXoffFlow := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetOutx_CtsFlow(const Value: Boolean);
begin
  if FOutx_CtsFlow <> Value then
  begin
    FOutx_CtsFlow := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetOutx_DsrFlow(const Value: Boolean);
begin
  if FOutx_DsrFlow <> Value then
  begin
    FOutx_DsrFlow := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetOutx_XonXoffFlow(const Value: Boolean);
begin
  if FOutx_XonXoffFlow <> Value then
  begin
    FOutx_XonXoffFlow := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetParityCheck(const Value: Boolean);
begin
  if FParityCheck <> Value then
  begin
    FParityCheck := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetReplacedChar(const Value: Char);
begin
  if FReplacedChar <> Value then
  begin
    FReplacedChar := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetReplaceWhenParityError(const Value: Boolean);
begin
  if FReplaceWhenParityError <> Value then
  begin
    FReplaceWhenParityError := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetRtsControl(const Value: TRtsControl);
begin
  if FRtsControl <> Value then
  begin
    FRtsControl := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetStopBits(const Value: TStopBits);
begin
  if FStopBits <> Value then
  begin
    FStopBits := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetTxContinueOnXoff(const Value: Boolean);
begin
  if FTxContinueOnXoff <> Value then
  begin
    FTxContinueOnXoff := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetXoffChar(const Value: Char);
begin
  if FXonChar = Value then
    raise ERS232Error.Create(SInvalidXonXoffChar);
  if FXoffChar <> Value then
  begin
    FXoffChar := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetXoffLimit(const Value: WORD);
begin
  if FXoffLimit <> Value then
  begin
    FXoffLimit := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetXonChar(const Value: Char);
begin
  if FXoffChar = Value then
    raise ERS232Error.Create(SInvalidXonXoffChar);
  if FXonChar <> Value then
  begin
    FXonChar := Value;
    Changed;
  end;
end;

procedure TCnRS232Config.SetXonLimit(const Value: WORD);
begin
  if FXonLimit <> Value then
  begin
    FXonLimit := Value;
    Changed;
  end;
end;

{ TCnRS232Timeouts }

procedure TCnRS232Timeouts.Assign(Source: TPersistent);
begin
  if Source is TCnRS232Timeouts then
  begin
    FReadIntervalTimeout := TCnRS232Timeouts(Source).FReadIntervalTimeout;
    FReadTotalTimeoutMultiplier := TCnRS232Timeouts(Source).FReadTotalTimeoutMultiplier;
    FReadTotalTimeoutConstant := TCnRS232Timeouts(Source).FReadTotalTimeoutConstant;
    FWriteTotalTimeoutMultiplier := TCnRS232Timeouts(Source).FWriteTotalTimeoutMultiplier;
    FWriteTotalTimeoutConstant := TCnRS232Timeouts(Source).FWriteTotalTimeoutConstant;
    Changed;
  end
  else
    inherited;
end;

constructor TCnRS232Timeouts.Create;
begin
  inherited Create;
  FReadIntervalTimeout := 10;
  FReadTotalTimeoutMultiplier := 0;
  FReadTotalTimeoutConstant := 0;
  FWriteTotalTimeoutMultiplier := 0;
  FWriteTotalTimeoutConstant := 0;
end;

function TCnRS232Timeouts.GetCommTimeouts: TCommTimeouts;
begin
  Result.ReadIntervalTimeout := FReadIntervalTimeout;
  Result.ReadTotalTimeoutMultiplier := FReadTotalTimeoutMultiplier;
  Result.ReadTotalTimeoutConstant := FReadTotalTimeoutConstant;
  Result.WriteTotalTimeoutMultiplier := FWriteTotalTimeoutMultiplier;
  Result.WriteTotalTimeoutConstant := FWriteTotalTimeoutConstant;
end;

procedure TCnRS232Timeouts.SetCommTimeouts(const Value: TCommTimeouts);
begin
  FReadIntervalTimeout := Value.ReadIntervalTimeout;
  FReadTotalTimeoutMultiplier := Value.ReadTotalTimeoutMultiplier;
  FReadTotalTimeoutConstant := Value.ReadTotalTimeoutConstant;
  FWriteTotalTimeoutMultiplier := Value.WriteTotalTimeoutMultiplier;
  FWriteTotalTimeoutConstant := Value.WriteTotalTimeoutConstant;
end;

procedure TCnRS232Timeouts.SetReadIntervalTimeout(const Value: DWord);
begin
  if FReadIntervalTimeout <> Value then
  begin
    FReadIntervalTimeout := Value;
    Changed;
  end;
end;

procedure TCnRS232Timeouts.SetReadTotalTimeoutConstant(const Value: DWord);
begin
  if FReadTotalTimeoutConstant <> Value then
  begin
    FReadTotalTimeoutConstant := Value;
    Changed;
  end;
end;

procedure TCnRS232Timeouts.SetReadTotalTimeoutMultiplier(const Value: DWord);
begin
  if FReadTotalTimeoutMultiplier <> Value then
  begin
    FReadTotalTimeoutMultiplier := Value;
    Changed;
  end;
end;

procedure TCnRS232Timeouts.SetWriteTotalTimeoutConstant(const Value: DWord);
begin
  if FWriteTotalTimeoutConstant <> Value then
  begin
    FWriteTotalTimeoutConstant := Value;
    Changed;
  end;
end;

procedure TCnRS232Timeouts.SetWriteTotalTimeoutMultiplier(
  const Value: DWord);
begin
  if FWriteTotalTimeoutMultiplier <> Value then
  begin
    FWriteTotalTimeoutMultiplier := Value;
    Changed;
  end;
end;

{ TCnRS232 }

procedure TCnRS232.Assign(Source: TPersistent);
var
  Save: Boolean;
begin
  if Source is TCnRS232 then
  begin
    Save := hCommFile <> 0;
    if Save then StopComm;
    FCommName := TCnRS232(Source).FCommName;
    FCommConfig.Assign(TCnRS232(Source).FCommConfig);
    FTimeouts.Assign(TCnRS232(Source).FTimeouts);
    if Save then StartComm;
  end
  else
    inherited;
end;

constructor TCnRS232.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommName := 'COM2';
  FCommConfig := TCnRS232Config.Create(ConfigChanged);
  FTimeouts := TCnRS232Timeouts.Create(TimeoutsChanged);
  ReadThread := nil;
  WriteThread := nil;
  hCommFile := 0;
  hCloseEvent := 0;
  FSendDataEmpty := True;

  if not (csDesigning in ComponentState) then
    FHWnd := AllocateHWnd(CommWndProc)
end;

destructor TCnRS232.Destroy;
begin
  StopComm;

  if not (csDesigning in ComponentState) then
    DeallocateHWnd(FHWnd);

  FCommConfig.Free;
  FTimeouts.Free;
  inherited Destroy;
end;

procedure TCnRS232.StartComm;
var
  hNewCommFile: THandle;
begin
  if (hCommFile <> 0) then
    raise ERS232Error.Create(SSerialPortAlreadyOpened);

  // 解决串口号大于10无法识别的问题
  hNewCommFile := CreateFile(PChar('\\.\' + CommName), GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, 0);

  if hNewCommFile = INVALID_HANDLE_VALUE then
    raise ERS232Error.Create(SSerialPortOpenError);

  if GetFileType(hNewCommFile) <> FILE_TYPE_CHAR then
  begin
    CloseHandle(hNewCommFile);
    raise ERS232Error.Create(SNotACommHandle);
  end;

  if not SetupComm(hNewCommFile, 4096, 4096) then
  begin
    CloseHandle(hCommFile);
    raise ERS232Error.Create(SSetupBuffFail);
  end;

  hCommFile := hNewCommFile;

  PurgeComm(hCommFile, PURGE_TXABORT or PURGE_RXABORT or
    PURGE_TXCLEAR or PURGE_RXCLEAR);
  FSendDataEmpty := True;

  _SetCommTimeout;

  _SetCommState;

  hCloseEvent := CreateEvent(nil, True, False, nil);

  if hCloseEvent = 0 then
  begin
    CloseHandle(hCommFile);
    hCommFile := 0;
    raise ERS232Error.Create(SCreateEventFail);
  end;

  try
    ReadThread := TReadThread.Create(True {suspended});
  except
    ReadThread := nil;
    CloseHandle(hCloseEvent);
    CloseHandle(hCommFile);
    hCommFile := 0;
    raise ERS232Error.Create(SCreateReadFail)
  end;
  ReadThread.hCommFile := hCommFile;
  ReadThread.hCloseEvent := hCloseEvent;
  ReadThread.hComm32Window := FHWnd;

  ReadThread.Priority := tpHighest;

  try
    WriteThread := TWriteThread.Create(True {suspended});
  except
    CloseReadThread;
    WriteThread := nil;
    CloseHandle(hCloseEvent);
    CloseHandle(hCommFile);
    hCommFile := 0;
    raise ERS232Error.Create(SCreateWriteFail);
  end;

  WriteThread.hCommFile := hCommFile;
  WriteThread.hCloseEvent := hCloseEvent;
  WriteThread.hComm32Window := FHWnd;
  WriteThread.pFSendDataEmpty := @FSendDataEmpty;

  WriteThread.Priority := tpHigher;

  ReadThread.Resume;
  WriteThread.Resume;
end;

procedure TCnRS232.StopComm;
begin
  if hCommFile = 0 then
    Exit;

  CloseReadThread;
  CloseWriteThread;

  CloseHandle(hCloseEvent);
  CloseHandle(hCommFile);
  hCommFile := 0;
end;

function TCnRS232.WriteCommData(pDataToWrite: PAnsiChar; dwSizeofDataToWrite: WORD): Boolean;
var
  Buffer: Pointer;
begin
  if (WriteThread <> nil) and (dwSizeofDataToWrite <> 0) then
  begin
    Buffer := Pointer(LocalAlloc(LPTR, dwSizeofDataToWrite + 1));
    Move(pDataToWrite^, Buffer^, dwSizeofDataToWrite);
    if PostThreadMessage(WriteThread.ThreadID, PWM_COMMWRITE,
      WPARAM(dwSizeofDataToWrite), LPARAM(Buffer)) then
    begin
      FSendDataEmpty := False;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TCnRS232.GetModemState: DWord;
var
  dwModemState: DWord;
begin
  if not GetCommModemStatus(hCommFile, dwModemState) then
    Result := 0
  else
    Result := dwModemState;
end;

procedure TCnRS232.CloseReadThread;
begin
  if ReadThread <> nil then
  begin
    SetEvent(hCloseEvent);
    PurgeComm(hCommFile, PURGE_RXABORT + PURGE_RXCLEAR);
    if (WaitForSingleObject(ReadThread.Handle, 10000) = WAIT_TIMEOUT) then
      ReadThread.Terminate;
    ReadThread.Free;
    ReadThread := nil;
  end;
end;

procedure TCnRS232.CloseWriteThread;
begin
  if WriteThread <> nil then
  begin
    SetEvent(hCloseEvent);
    PurgeComm(hCommFile, PURGE_TXABORT + PURGE_TXCLEAR);
    FSendDataEmpty := True;
    if WaitForSingleObject(WriteThread.Handle, 10000) = WAIT_TIMEOUT then
      WriteThread.Terminate;
    WriteThread.Free;
    WriteThread := nil;
  end;
end;

procedure TCnRS232.ReceiveData(Buffer: PAnsiChar; BufferLength: WORD);
begin
  if Assigned(FOnReceiveData) then
    FOnReceiveData(Self, Buffer, BufferLength)
end;

procedure TCnRS232.ReceiveError(EvtMask: DWord);
begin
  if Assigned(FOnReceiveError) then
    FOnReceiveError(Self, EvtMask)
end;

procedure TCnRS232.ModemStateChange(ModemEvent: DWord);
begin
  if Assigned(FOnModemStateChange) then
    FOnModemStateChange(Self, ModemEvent)
end;

procedure TCnRS232.RequestHangup;
begin
  if Assigned(FOnRequestHangup) then
    FOnRequestHangup(Self)
end;

procedure TCnRS232._SendDataEmpty;
begin
  if Assigned(FOnSendDataEmpty) then
    FOnSendDataEmpty(Self)
end;

procedure TCnRS232.CommWndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    PWM_GOTCOMMDATA:
      begin
        ReceiveData(PAnsiChar(Msg.LPARAM), Msg.WPARAM);
        LocalFree(Msg.LPARAM)
      end;
    PWM_RECEIVEERROR: ReceiveError(Msg.LPARAM);
    PWM_MODEMSTATECHANGE: ModemStateChange(Msg.LPARAM);
    PWM_REQUESTHANGUP: RequestHangup;
    PWM_SENDDATAEMPTY: _SendDataEmpty;
  else
    with msg do //默认消息处理,解决win98无法正常退出问题
               //（WM_QUERYENDSESSION和WM_ENDSESSION）
      Result := DefWindowProc(FHWnd, Msg, WParam, LParam);
  end;
end;

procedure TCnRS232._SetCommState;
var
  DCB: TDCB;
  commprop: TCommProp;
  fdwEvtMask: DWord;
begin
  GetCommState(hCommFile, DCB);
  GetCommProperties(hCommFile, commprop);
  GetCommMask(hCommFile, fdwEvtMask);
  FCommConfig.GetDCB(DCB);
  SetCommState(hCommFile, DCB);
end;

procedure TCnRS232._SetCommTimeout;
var
  CommTimeouts: TCommTimeouts;
begin
  //GetCommTimeouts(hCommFile, CommTimeouts);
  CommTimeouts := Timeouts.GetCommTimeouts;
  SetCommTimeouts(hCommFile, CommTimeouts);
end;

procedure TCnRS232.ConfigChanged(Sender: TObject);
begin
  _SetCommState;
end;

procedure TCnRS232.TimeoutsChanged(Sender: TObject);
begin
  _SetCommTimeout;
end;

function TCnRS232.GetConnected: Boolean;
begin
  Result := hCommFile <> 0;
end;

procedure TCnRS232.SetCommConfig(const Value: TCnRS232Config);
begin
  FCommConfig.Assign(Value);
end;

procedure TCnRS232.SetTimeouts(const Value: TCnRS232Timeouts);
begin
  FTimeouts.Assign(Value);
end;

const
  csCommName = 'CommName';
  csXoffChar = 'XoffChar';
  csReplacedChar = 'ReplacedChar';
  csXonChar = 'XonChar';
  csOutx_CtsFlow = 'Outx_CtsFlow';
  csOutx_DsrFlow = 'Outx_DsrFlow';
  csParityCheck = 'ParityCheck';
  csIgnoreNullChar = 'IgnoreNullChar';
  csInx_XonXoffFlow = 'Inx_XonXoffFlow';
  csTxContinueOnXoff = 'TxContinueOnXoff';
  csReplaceWhenParityError = 'ReplaceWhenParityError';
  csOutx_XonXoffFlow = 'Outx_XonXoffFlow';
  csDsrSensitivity = 'DsrSensitivity';
  csBaudRate = 'BaudRate';
  csByteSize = 'ByteSize';
  csDtrControl = 'DtrControl';
  csParity = 'Parity';
  csRtsControl = 'RtsControl';
  csStopBits = 'StopBits';
  csXoffLimit = 'XoffLimit';
  csXonLimit = 'XonLimit';
  csReadIntervalTimeout = 'ReadIntervalTimeout';
  csReadTotalTimeoutConstant = 'ReadTotalTimeoutConstant';
  csReadTotalTimeoutMultiplier = 'ReadTotalTimeoutMultiplier';
  csWriteTotalTimeoutMultiplier = 'WriteTotalTimeoutMultiplier';
  csWriteTotalTimeoutConstant = 'WriteTotalTimeoutConstant';

procedure TCnRS232.ReadFromIni(Ini: TCustomIniFile; const Section: string);
begin
  FCommName := Ini.ReadString(Section, csCommName, FCommName);
  with FCommConfig do
  begin
    FXoffChar := Char(Ini.ReadInteger(Section, csXoffChar, Byte(FXoffChar)));
    FReplacedChar := Char(Ini.ReadInteger(Section, csReplacedChar, Byte(FReplacedChar)));
    FXonChar := Char(Ini.ReadInteger(Section, csXonChar, Byte(FXonChar)));
    FOutx_CtsFlow := Ini.ReadBool(Section, csOutx_CtsFlow, FOutx_CtsFlow);
    FOutx_DsrFlow := Ini.ReadBool(Section, csOutx_DsrFlow, FOutx_DsrFlow);
    FParityCheck := Ini.ReadBool(Section, csParityCheck, FParityCheck);
    FIgnoreNullChar := Ini.ReadBool(Section, csIgnoreNullChar, FIgnoreNullChar);
    FInx_XonXoffFlow := Ini.ReadBool(Section, csInx_XonXoffFlow, FInx_XonXoffFlow);
    FTxContinueOnXoff := Ini.ReadBool(Section, csTxContinueOnXoff, FTxContinueOnXoff);
    FReplaceWhenParityError := Ini.ReadBool(Section, csReplaceWhenParityError, FReplaceWhenParityError);
    FOutx_XonXoffFlow := Ini.ReadBool(Section, csOutx_XonXoffFlow, FOutx_XonXoffFlow);
    FDsrSensitivity := Ini.ReadBool(Section, csDsrSensitivity, FDsrSensitivity);
    FBaudRate := Ini.ReadInteger(Section, csBaudRate, FBaudRate);
    FByteSize := TByteSize(Ini.ReadInteger(Section, csByteSize, Ord(FByteSize)));
    FDtrControl := TDtrControl(Ini.ReadInteger(Section, csDtrControl, Ord(FDtrControl)));
    FParity := TParity(Ini.ReadInteger(Section, csParity, Ord(FParity)));
    FRtsControl := TRtsControl(Ini.ReadInteger(Section, csRtsControl, Ord(FRtsControl)));
    FStopBits := TStopBits(Ini.ReadInteger(Section, csStopBits, Ord(FStopBits)));
    FXoffLimit := Ini.ReadInteger(Section, csXoffLimit, FXoffLimit);
    FXonLimit := Ini.ReadInteger(Section, csXonLimit, FXonLimit);
  end;
  with FTimeouts do
  begin
    FReadTotalTimeoutConstant := Ini.ReadInteger(Section, csReadTotalTimeoutConstant, FReadTotalTimeoutConstant);
    FReadIntervalTimeout := Ini.ReadInteger(Section, csReadIntervalTimeout, FReadIntervalTimeout);
    FReadTotalTimeoutMultiplier := Ini.ReadInteger(Section, csReadTotalTimeoutMultiplier, FReadTotalTimeoutMultiplier);
    FWriteTotalTimeoutConstant := Ini.ReadInteger(Section, csWriteTotalTimeoutConstant, FWriteTotalTimeoutConstant);
    FWriteTotalTimeoutMultiplier := Ini.ReadInteger(Section, csWriteTotalTimeoutMultiplier, FWriteTotalTimeoutMultiplier);
  end;
end;

procedure TCnRS232.WriteToIni(Ini: TCustomIniFile; const Section: string);
begin
  Ini.WriteString(Section, csCommName, FCommName);
  with FCommConfig do
  begin
    Ini.WriteInteger(Section, csXoffChar, Byte(FXoffChar));
    Ini.WriteInteger(Section, csReplacedChar, Byte(FReplacedChar));
    Ini.WriteInteger(Section, csXonChar, Byte(FXonChar));
    Ini.WriteBool(Section, csOutx_CtsFlow, FOutx_CtsFlow);
    Ini.WriteBool(Section, csOutx_DsrFlow, FOutx_DsrFlow);
    Ini.WriteBool(Section, csParityCheck, FParityCheck);
    Ini.WriteBool(Section, csIgnoreNullChar, FIgnoreNullChar);
    Ini.WriteBool(Section, csInx_XonXoffFlow, FInx_XonXoffFlow);
    Ini.WriteBool(Section, csTxContinueOnXoff, FTxContinueOnXoff);
    Ini.WriteBool(Section, csReplaceWhenParityError, FReplaceWhenParityError);
    Ini.WriteBool(Section, csOutx_XonXoffFlow, FOutx_XonXoffFlow);
    Ini.WriteBool(Section, csDsrSensitivity, FDsrSensitivity);
    Ini.WriteInteger(Section, csBaudRate, FBaudRate);
    Ini.WriteInteger(Section, csByteSize, Ord(FByteSize));
    Ini.WriteInteger(Section, csDtrControl, Ord(FDtrControl));
    Ini.WriteInteger(Section, csParity, Ord(FParity));
    Ini.WriteInteger(Section, csRtsControl, Ord(FRtsControl));
    Ini.WriteInteger(Section, csStopBits, Ord(FStopBits));
    Ini.WriteInteger(Section, csXoffLimit, FXoffLimit);
    Ini.WriteInteger(Section, csXonLimit, FXonLimit);
  end;

  with FTimeouts do
  begin
    Ini.WriteInteger(Section, csReadTotalTimeoutConstant, FReadTotalTimeoutConstant);
    Ini.WriteInteger(Section, csReadTotalTimeoutMultiplier, FReadTotalTimeoutMultiplier);
    Ini.WriteInteger(Section, csReadIntervalTimeout, FReadIntervalTimeout);
    Ini.WriteInteger(Section, csWriteTotalTimeoutMultiplier, FWriteTotalTimeoutMultiplier);
    Ini.WriteInteger(Section, csWriteTotalTimeoutConstant, FWriteTotalTimeoutConstant);
  end;
end;

procedure TCnRS232.ReadFromIni(const FileName, Section: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    ReadFromIni(Ini, Section);
  finally
    Ini.Free;
  end;
end;

procedure TCnRS232.WriteToIni(const FileName, Section: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    WriteToIni(Ini, Section);
  finally
    Ini.Free;
  end;
end;

procedure TCnRS232.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnRS232Name;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnRS232Comment;
end;

end.

