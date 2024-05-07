{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnConsts;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：公共资源字符串定义单元
* 单元作者：CnPack 开发组
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2004.09.18 V1.2
*                新增CnMemProf的字符串定义
*           2002.04.18 V1.1
*                新增部分字符串定义
*           2002.04.08 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

const
  ECN_OK                              = 0;         // 错误码 OK，无错误

  ECN_FILE_NOT_FOUND                  = $10;       // 文件不存在

  ECN_CUSTOM_ERROR_BASE               = $1000;     // 供外界设定的错误码起始值

//==============================================================================
// 不需要本地化的字符串
//==============================================================================

resourcestring

  // 注册表路径
  SCnPackRegPath = '\Software\CnPack';

  // 辅助工具路径
  SCnPackToolRegPath = 'CnTools';

//==============================================================================
// 需要本地化的字符串
//==============================================================================


var
  // 公共信息
  SCnInformation: string = '提示';
  SCnWarning: string = '警告';
  SCnError: string = '错误';
  SCnEnabled: string = '有效';
  SCnDisabled: string = '禁用';
  SCnMsgDlgOK: string = '确认(&O)';
  SCnMsgDlgCancel: string = '取消(&C)';
  SCnMsgDlgYes: string = '是(&Y)';
  SCnMsgDlgNo: string = '否(&N)';
  SCnMsgDlgYesToAll: string = '全是(&A)';
  SCnMsgDlgNoToAll: string = '全否(&l)';

const
  // 开发包信息
  SCnPackAbout = 'CnPack';
  SCnPackVer = 'Ver 0.1.3.1';
  SCnPackStr = SCnPackAbout + ' ' + SCnPackVer;
  SCnPackUrl = 'https://www.cnpack.org';
  SCnPackBbsUrl = 'https://bbs.cnpack.org';
  SCnPackNewsUrl = 'news://news.cnpack.org';
  SCnPackSourceUrl = 'https://github.com/cnpack';
  SCnPackEmail = 'master@cnpack.org';
  SCnPackBugEmail = 'bugs@cnpack.org';
  SCnPackSuggestionsEmail = 'suggestions@cnpack.org';

  SCnPackDonationUrl = 'https://www.cnpack.org/foundation.php';
  SCnPackDonationUrlSF = 'http://sourceforge.net/donate/index.php?group_id=110999';
  SCnPackGroup = 'CnPack 开发组';
  SCnPackCopyright = '(C)Copyright 2001-2024 ' + SCnPackGroup;

  // CnPropEditors
  SCopyrightFmtStr =
    SCnPackStr + #13#10#13#10 +
    '组件名称: %s' + #13#10 +
    '组件作者: %s(%s)' + #13#10 +
    '组件说明: %s' + #13#10#13#10 +
    '下载网站: ' + SCnPackUrl + #13#10 +
    '技术支持: ' + SCnPackEmail + #13#10#13#10 +
    SCnPackCopyright;

resourcestring

  // 组件安装面板名
  SCnNonVisualPalette = 'CnPack Tools';
  SCnGraphicPalette = 'CnPack VCL';
  SCnNetPalette = 'CnPack Net';
  SCnDatabasePalette = 'CnPack DB';
  SCnReportPalette = 'CnPack Report';

  // 开发组成员信息请在后面添加，注意本地化处理
var
  SCnPack_Team: string = 'CnPack开发组';
  SCnPack_Zjy: string = '周劲羽';
  SCnPack_Shenloqi: string = '沈龙强(Chinbo)';
  SCnPack_xiaolv: string = '吕宏庆';
  SCnPack_Flier: string = 'Flier Lu';
  SCnPack_LiuXiao: string = '刘啸(Passion)';
  SCnPack_PanYing: string = '潘鹰(Pan Ying)';
  SCnPack_Hubdog: string = '陈省(Hubdog)';
  SCnPack_Wyb_star: string = '王玉宝';
  SCnPack_Licwing: string = '朱磊(Licwing Zue)';
  SCnPack_Alan: string = '张伟(Alan)';
  SCnPack_GuYueChunQiu: string = '古月春秋';
  SCnPack_Aimingoo: string = '周爱民(Aimingoo)';
  SCnPack_QSoft: string = '何清(QSoft)';
  SCnPack_Hospitality: string = '张炅轩(Hospitality)';
  SCnPack_SQuall: string = '刘玺(SQUALL)';
  SCnPack_Hhha: string = 'Hhha';
  SCnPack_Beta: string = '熊恒(beta)';
  SCnPack_Leeon: string = '李柯(Leeon)';
  SCnPack_SuperYoyoNc: string = '许子健';
  SCnPack_JohnsonZhong: string = 'Johnson Zhong';
  SCnPack_DragonPC: string = 'Dragon P.C.';
  SCnPack_Kendling: string = '小冬(Kending)';
  SCnPack_ccrun: string = 'ccRun(老妖)';
  SCnPack_Dingbaosheng: string = 'dingbaosheng';
  SCnPack_LuXiaoban: string = '周益波(鲁小班)';
  SCnPack_Savetime: string = 'savetime';
  SCnPack_solokey: string = 'solokey';
  SCnPack_Bahamut: string = '巴哈姆特';
  SCnPack_Sesame: string = '胡昌洪(Sesame)';
  SCnPack_BuDeXian: string = '不得闲';
  SCnPack_XiaoXia: string = '小夏';
  SCnPack_ZiMin: string = '子旻';
  SCnPack_rarnu: string = 'rarnu';
  SCnPack_dejoy: string = 'dejoy';
  SCnPack_Rain: string = 'Rain';
  SCnPack_cnwinds: string = 'cnwinds';

  // CnCommon
  SUnknowError: string = '未知错误';
  SErrorCode: string = '错误代码：';

const
  SCnPack_TeamEmail = 'master@cnpack.org';
  SCnPack_ZjyEmail = 'zjy@cnpack.org';
  SCnPack_ShenloqiEmail = 'Shenloqi@hotmail.com';
  SCnPack_xiaolvEmail = 'xiaolv888@etang.com';
  SCnPack_FlierEmail = 'flier_lu@sina.com';
  SCnPack_LiuXiaoEmail = 'liuxiao@cnpack.org';
  SCnPack_PanYingEmail = 'panying@sina.com';
  SCnPack_HubdogEmail = 'hubdog@263.net';
  SCnPack_Wyb_starMail = 'wyb_star@sina.com';
  SCnPack_LicwingEmail = 'licwing@chinasystemsn.com';
  SCnPack_AlanEmail = 'BeyondStudio@163.com';
  SCnPack_GuYueChunQiuEmail = 'guyuechunqiu@cnpack.org';
  SCnPack_AimingooEmail = 'aim@263.net';
  SCnPack_QSoftEmail = 'hq.com@263.net';
  SCnPack_HospitalityEmail = 'Hospitality_ZJX@msn.com';
  SCnPack_SQuallEmail = 'squall_sa@163.com';
  SCnPack_HhhaEmail = 'Hhha@eyou.com';
  SCnPack_BetaEmail = 'beta@01cn.net';
  SCnPack_LeeonEmail = 'real-like@163.com';
  SCnPack_SuperYoyoNcEmail = 'superyoyonc@sohu.com';
  SCnPack_JohnsonZhongEmail = 'zhongs@tom.com';
  SCnPack_DragonPCEmail = 'dragonpc@21cn.com';
  SCnPack_KendlingEmail = 'kendling@21cn.com';
  SCnPack_ccRunEmail = 'info@ccrun.com';
  SCnPack_DingbaoshengEmail = 'yzdbs@msn.com';
  SCnPack_LuXiaobanEmail = 'zhouyibo2000@sina.com';
  SCnPack_SavetimeEmail = 'savetime2k@hotmail.com';
  SCnPack_solokeyEmail = 'crh611@163.com';
  SCnPack_BahamutEmail = 'fantasyfinal@126.com';
  SCnPack_SesameEmail = 'sesamehch@163.com';
  SCnPack_BuDeXianEmail = 'appleak46@yahoo.com.cn';
  SCnPack_XiaoXiaEmail = 'summercore@163.com';
  SCnPack_ZiMinEmail = '441414288@qq.com';
  SCnPack_rarnuEmail = 'rarnu@cnpack.org';
  SCnPack_dejoyEmail = 'dejoybbs@163.com';
  SCnPack_RainEmail = SCnPack_TeamEmail;
  SCnPack_cnwindsEmail = SCnPack_TeamEmail;

  // CnMemProf
  SCnPackMemMgr = '内存管理监视器';
  SMemLeakDlgReport = '出现 %d 处内存漏洞[替换内存管理器之前已分配 %d 处]。';
  SMemMgrODSReport = '获取 = %d，释放 = %d，重分配 = %d';
  SMemMgrOverflow = '内存管理监视器指针列表溢出，请增大列表项数！';
  SMemMgrRunTime = '%d 小时 %d 分 %d 秒。';
  SOldAllocMemCount = '替换内存管理器前已分配 %d 处内存。';
  SAppRunTime = '程序运行时间: ';
  SMemSpaceCanUse = '可用地址空间: %d 千字节';
  SUncommittedSpace = '未提交部分: %d 千字节';
  SCommittedSpace = '已提交部分: %d 千字节';
  SFreeSpace = '空闲部分: %d 千字节';
  SAllocatedSpace = '已分配部分: %d 千字节';
  SAllocatedSpacePercent = '地址空间载入: %d%%';
  SFreeSmallSpace = '全部小空闲内存块: %d 千字节';
  SFreeBigSpace = '全部大空闲内存块: %d 千字节';
  SUnusedSpace = '其它未用内存块: %d 千字节';
  SOverheadSpace = '内存管理器消耗: %d 千字节';
  SObjectCountInMemory = '内存对象数目: ';
  SNoMemLeak = '没有内存泄漏。';
  SNoName = '(未命名)';
  SNotAnObject = '不是对象';
  SByte = '字节';
  SCommaString = '，';
  SPeriodString = '。';

function CnGetLastError: Integer;

procedure _CnSetLastError(Err: Integer);

implementation

threadvar
  CnErrorCode: Integer;

function CnGetLastError: Integer;
begin
  Result := CnErrorCode;
end;

procedure _CnSetLastError(Err: Integer);
begin
  CnErrorCode := Err;
end;

end.

