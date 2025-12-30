{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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
  ECN_OK                              = 0;         // 岿粇絏 OK礚岿粇

  ECN_FILE_NOT_FOUND                  = $10;       // ゅンぃ

  ECN_CUSTOM_ERROR_BASE               = $1000;     // ㄑ砞﹚岿粇絏癬﹍

//==============================================================================
// ぃ惠璶セて才﹃
//==============================================================================

resourcestring

  // 爹隔畖
  SCnPackRegPath = '\Software\CnPack';

  // 徊ㄣ隔畖
  SCnPackToolRegPath = 'CnTools';

//==============================================================================
// 惠璶セて才﹃
//==============================================================================


var
  // そ獺
  SCnHint: string = '矗ボ';
  SCnInformation: string = '獺';
  SCnWarning: string = '牡';
  SCnError: string = '岿粇';
  SCnEnabled: string = 'Τ';
  SCnDisabled: string = '窽ノ';
  SCnMsgDlgOK: string = '絋粄(&O)';
  SCnMsgDlgCancel: string = '(&C)';
  SCnMsgDlgYes: string = '琌(&Y)';
  SCnMsgDlgNo: string = '(&N)';
  SCnMsgDlgYesToAll: string = '琌(&A)';
  SCnMsgDlgNoToAll: string = '(&l)';
  SCnVersion: string = 'セ';
  SCnNeedAdmin: string = '惠璶恨瞶舦';
  SCnNotSupport: string = '赣巨ぃや';

const
  // 秨祇獺
  SCnPackAbout = 'CnPack';
  SCnPackVer = 'Ver 0.1.7.1';
  SCnPackStr = SCnPackAbout + ' ' + SCnPackVer;
  SCnPackUrl = 'https://www.cnpack.org';
  SCnPackBbsUrl = 'https://bbs.cnpack.org';
  SCnPackNewsUrl = 'news://news.cnpack.org';
  SCnPackSourceUrl = 'https://github.com/cnpack';
  SCnPackEmail = 'master@cnpack.org';
  SCnPackBugEmail = 'bugs@cnpack.org';
  SCnPackSuggestionsEmail = 'suggestions@cnpack.org';

  SCnPackDonationUrl = 'https://www.cnpack.org/donation.php';
  SCnPackDonationUrlSF = 'http://sourceforge.net/donate/index.php?group_id=110999';
  SCnPackGroup = 'CnPack 秨祇舱';
  SCnPackCopyright = '(C)Copyright 2001-2025 ' + SCnPackGroup;

  // CnPropEditors
  SCopyrightFmtStr =
    SCnPackStr + #13#10#13#10 +
    '舱ン嘿: %s' + #13#10 +
    '舱ン: %s(%s)' + #13#10 +
    '舱ン弧: %s' + #13#10#13#10 +
    '更呼: ' + SCnPackUrl + #13#10 +
    'м砃や: ' + SCnPackEmail + #13#10#13#10 +
    SCnPackCopyright;

resourcestring

  // 舱ン杆狾
  SCnNonVisualPalette = 'CnPack Tools';
  SCnGraphicPalette = 'CnPack VCL';
  SCnNetPalette = 'CnPack Net';
  SCnDatabasePalette = 'CnPack DB';
  SCnReportPalette = 'CnPack Report';

  // 秨祇舱Θ獺叫睰猔種セて矪瞶
var
  SCnPack_Team: string = 'CnPack秨祇舱';
  SCnPack_Zjy: string = '㏄玪π';
  SCnPack_Shenloqi: string = '℉纒眏(Chinbo)';
  SCnPack_xiaolv: string = 'Щ紋';
  SCnPack_Flier: string = 'Flier Lu';
  SCnPack_LiuXiao: string = '糂糞(Passion)';
  SCnPack_PanYing: string = '硷芅(Pan Ying)';
  SCnPack_Hubdog: string = '朝(Hubdog)';
  SCnPack_Wyb_star: string = 'ド腳';
  SCnPack_Licwing: string = 'Χ経(Licwing Zue)';
  SCnPack_Alan: string = '眎岸(Alan)';
  SCnPack_GuYueChunQiu: string = 'る琄';
  SCnPack_Aimingoo: string = '㏄稲チ(Aimingoo)';
  SCnPack_QSoft: string = '睲(QSoft)';
  SCnPack_Hospitality: string = '眎蚏癮(Hospitality)';
  SCnPack_SQuall: string = '糂芒(SQUALL)';
  SCnPack_Hhha: string = 'Hhha';
  SCnPack_Beta: string = '旱(beta)';
  SCnPack_Leeon: string = '琠(Leeon)';
  SCnPack_SuperYoyoNc: string = '砛胺';
  SCnPack_JohnsonZhong: string = 'Johnson Zhong';
  SCnPack_DragonPC: string = 'Dragon P.C.';
  SCnPack_Kendling: string = '(Kending)';
  SCnPack_ccrun: string = 'ccRun(ρН)';
  SCnPack_Dingbaosheng: string = 'dingbaosheng';
  SCnPack_LuXiaoban: string = '㏄痲猧(緗痁)';
  SCnPack_Savetime: string = 'savetime';
  SCnPack_solokey: string = 'solokey';
  SCnPack_Bahamut: string = 'ぺ﹊疭';
  SCnPack_Sesame: string = '璊瑇(Sesame)';
  SCnPack_BuDeXian: string = 'ぃ眔盯';
  SCnPack_XiaoXia: string = '甃';
  SCnPack_ZiMin: string = '躺';
  SCnPack_rarnu: string = 'rarnu';
  SCnPack_dejoy: string = 'dejoy';
  SCnPack_Rain: string = 'Rain';
  SCnPack_cnwinds: string = 'cnwinds';

  // CnCommon
  SUnknowError: string = 'ゼ岿粇';
  SErrorCode: string = '岿粇絏';

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
  SCnPackMemMgr = 'ず恨瞶菏跌竟';
  SMemLeakDlgReport = '瞷 %d 矪ず簗瑌[蠢传ず恨瞶竟ぇ玡だ皌 %d 矪]';
  SMemMgrODSReport = '莉 = %d睦 = %dだ皌 = %d';
  SMemMgrOverflow = 'ず恨瞶菏跌竟皐犯叫糤兜计';
  SMemMgrRunTime = '%d  %d だ %d ';
  SOldAllocMemCount = '蠢传ず恨瞶竟玡だ皌 %d 矪ず';
  SAppRunTime = '祘笲︽丁: ';
  SMemSpaceCanUse = 'ノ丁: %d 竊';
  SUncommittedSpace = 'ゼ矗ユ场だ: %d 竊';
  SCommittedSpace = '矗ユ场だ: %d 竊';
  SFreeSpace = '盯场だ: %d 竊';
  SAllocatedSpace = 'だ皌场だ: %d 竊';
  SAllocatedSpacePercent = '丁更: %d%%';
  SFreeSmallSpace = '场盯ず遏: %d 竊';
  SFreeBigSpace = '场盯ず遏: %d 竊';
  SUnusedSpace = 'ㄤウゼノず遏: %d 竊';
  SOverheadSpace = 'ず恨瞶竟: %d 竊';
  SObjectCountInMemory = 'ず癸钩计ヘ: ';
  SNoMemLeak = '⊿Τず簗';
  SNoName = '(ゼ㏑)';
  SNotAnObject = 'ぃ琌癸钩';
  SByte = '竊';
  SCommaString = '';
  SPeriodString = '';

resourcestring
  SCnErrorMapViewOfFile = 'MapViewOfFile ア毖 ';
  SCnErrorCreateFileMapping = 'CreateFileMapping ア毖 ';

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

