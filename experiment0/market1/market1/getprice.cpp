#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <windows.h>
#include "ThostFtdcMdApi.h"


HANDLE g_hEvent=CreateEvent(NULL, true,false,NULL);

TThostFtdcBrokerIDType g_chBrokerID;
TThostFtdcUserIDType g_chUserID;

class CSimpleHandler : public CThostFtdcMdSpi {
private:
	int n;
	CThostFtdcMdApi *m_pUserApi;

public:
	CSimpleHandler(CThostFtdcMdApi *pUserApi) : m_pUserApi(pUserApi) ,n(0) {}
	~CSimpleHandler() {}

	virtual void OnFrontConnected(){
		//char tmp[1000];
		CThostFtdcReqUserLoginField reqUserLogin;
		printf("BrokerID:7030\n");
		sscanf("7030", "%s", &g_chBrokerID);
		strcpy(reqUserLogin.BrokerID , g_chBrokerID);
		printf("userid:\n");
		sscanf("*","%s", &g_chUserID);
		strcpy(reqUserLogin.UserID, g_chUserID);
		printf("password:\n");
		sscanf("*","%s", &reqUserLogin.Password);
		//scanf("%s", &tmp[0]);
		fflush(stdout);
		m_pUserApi->ReqUserLogin(&reqUserLogin,0);
	}

	virtual void OnFrontDisconnected(int nReason){
		printf("\nOnFrontDisconnected = [%d].\n", nReason);
	}

	virtual void OnRspUserLogin(CThostFtdcRspUserLoginField *pRspUserLogin, CThostFtdcRspInfoField *pRspInfo, int nRequestID, bool bIsLast){
		printf("OnRspUserLogin:\n");
		printf("ErrorCode[%d], ErrorMsg=[%s]\n", pRspInfo->ErrorID, pRspInfo->ErrorMsg);
		printf("RequestID=[%d], Chain=[%d]\n", nRequestID, bIsLast);
		if (pRspInfo->ErrorID != 0 ){
			printf("Failed to login, errorcode=%d errormsg=%s requestid=%d chain=%d",
				pRspInfo->ErrorID, pRspInfo->ErrorMsg, nRequestID, bIsLast );
			exit(-1);
		}
		char * ins1[]={
			"CF209","CF307","ER305","ME208","ME301",
"ME306","PM305","RO301","SR301","SR311",
"TA212","TA305","WS211","WT211","a1305",
"ag1209","ag1302","ag1307","al1212","al1305",
"au1210","au1303","b1209","b1307","c1305",
"cu1211","cu1304","fu1210","fu1304","j1208",
"j1301","j1306","l1211","l1304","m1209",
"m1305","p1211","p1304","pb1209","pb1302",
"pb1307","rb1212","rb1305","ru1210","ru1305",
"v1210","v1303","wr1208","wr1301","wr1306",
"y1212","zn1208","zn1301","zn1306","CF211",
"ER209","IF1208","ME209","ME302","ME307",
"PM307","RO303","SR303","TA208","TA301",
"TA306","WS301","a1209","a1307","ag1210",
"ag1303","al1208","al1301","al1306","au1211",
"au1304","b1211","c1209","c1307","cu1212",
"cu1305","fu1211","fu1305","j1209","j1302",
"j1307","l1212","l1305","m1211","m1307",
"p1212","p1305","pb1210","pb1303","rb1208",
"rb1301","rb1306","ru1211","ru1306","v1211",
"v1304","wr1209","wr1302","wr1307","y1301",
"zn1209","zn1302","zn1307","CF301","ER211",
"IF1209","ME210","ME303","OI307","RI307",
"RO305","SR305","TA209","TA302","TA307",
"WS303","a1211","a1309","ag1211","ag1304",
"al1209","al1302","al1307","au1212","au1305",
"b1301","c1211","cu1208","cu1301","cu1306",
"fu1212","fu1306","j1210","j1303","l1208",
"l1301","l1306","m1212","p1208","p1301",
"p1306","pb1211","pb1304","rb1209","rb1302",
"rb1307","ru1301","ru1307","v1212","v1305",
"wr1210","wr1303","y1208","y1303","zn1210",
"zn1303","","CF303","ER301","IF1212",
"ME211","ME304","PM301","RO209","SR209",
"SR307","TA210","TA303","WH307","WS305",
"a1301","a1311","ag1212","ag1305","al1210",
"al1303","au1208","au1301","au1306","b1303",
"c1301","cu1209","cu1302","cu1307","fu1301",
"fu1307","j1211","j1304","l1209","l1302",
"l1307","m1301","p1209","p1302","p1307",
"pb1212","pb1305","rb1210","rb1303","ru1208",
"ru1303","v1208","v1301","v1306","wr1211",
"wr1304","y1209","y1305","zn1211","zn1304",
"","CF305","ER303","IF1303","ME212",
"ME305","PM303","RO211","SR211","SR309",
"TA211","TA304","WS209","WT209","a1303",
"a1401","ag1301","ag1306","al1211","al1304",
"au1209","au1302","au1307","b1305","c1303",
"cu1210","cu1303","fu1209","fu1303","fu1308",
"j1212","j1305","l1210","l1303","m1208",
"m1303","p1210","p1303","pb1208","pb1301",
"pb1306","rb1211","rb1304","ru1209","ru1304",
"v1209","v1302","v1307","wr1212","wr1305",
"y1211","y1307","zn1212","zn1305"};
		char * Instrument[]={ "CF301" , "CF305", "IF1209", "IF1210","IF1212","SR301",
		"SR305","TA301","TA305"};
		m_pUserApi->SubscribeMarketData(ins1, 269);
		// m_pUserApi->UnSubscribeMarketData(Instrument,2);
	}
	virtual void OnRtnDepthMarketData( CThostFtdcDepthMarketDataField *pDepthMarketData) {
		if (pDepthMarketData!=NULL) {
			printf("n=[%d] T=[%s] ID=[%s] P=[%f]\n",
				n,
				pDepthMarketData->UpdateTime,
				pDepthMarketData->InstrumentID,
				pDepthMarketData->LastPrice
				);
			
		} else {
			printf("n=[%d] null\n");
		}
        if (n++ > 1000) SetEvent(g_hEvent);
	}

	virtual void OnRspError(CThostFtdcRspInfoField *pRspInfo, int nRequestID, bool bIsLast) {
		printf("OnRspError:\n");
		printf("ErrorCode=[%d], ErrorMsg=[%s]\n", pRspInfo->ErrorID, pRspInfo->ErrorMsg );
		printf("RequestID=[%d], Chain=[%d]\n", nRequestID, bIsLast);
	}

};

int main(){
	CThostFtdcMdApi *pUserApi = CThostFtdcMdApi::CreateFtdcMdApi();
	CSimpleHandler sh(pUserApi);
	pUserApi->RegisterSpi(&sh);
	pUserApi->RegisterFront("tcp://asp-sim2-md1.financial-trading-platform.com:26213");
	pUserApi->Init();

	WaitForSingleObject(g_hEvent, INFINITE);
	pUserApi->Release();
	return 0;
}


		

