/*
 * IPWorks SNMP 2024 C++ Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks SNMP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworkssnmp
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworkssnmp.h"
#define LINE_LEN 100


class MySNMP: public SNMPMgr
{
public:

	virtual int FireError(SNMPMgrErrorEventParams *e)
	{
		printf("Error %i: %s\n", e->ErrorCode, e->Description);
		return 0;
	}

};


int main(int argc, char **argv)
{

	if (argc < 4)
	{
		fprintf(stderr, "usage:    snmp <VERSION (1 | 2 | 3)> <AGENT> <OID> [USER] [AuthPass] [PrivPass]\n\n");
		fprintf(stderr, "examples: snmp 1 10.0.1.1 1.3.6.1.2.1.1.1\n");
		fprintf(stderr, "          snmp 3 10.0.1.1 1.3.6.1.2.1.1.1 myuser my_password\n");
		fprintf(stderr, "          snmp 3 10.0.1.1 1.3.6.1.2.1.1.1 desuser des_password des_password\n");
		fprintf(stderr, "\npress <return> to continue...\n");
		getchar();
		exit(1);
	}

	MySNMP snmp;
	int retcode;

	snmp.SetSNMPVersion(atoi(argv[1])); //1st argument should be 1, 2 (for v2c), or 3
	snmp.SetRemoteHost(argv[2]); //2nd argument should be the agent ip or hostname

	if (snmp.GetSNMPVersion() == SNMPVER_V3)
	{
		snmp.SetUser(argv[4]);
		snmp.SetAuthenticationPassword(argv[5]);
		snmp.SetEncryptionPassword(argv[6]);
		//SNMPv3 requires Discovery, so discover the agent first to retrieve its engineid
		retcode = snmp.Discover();
		if (retcode)
		{
			printf("Manager Error: %s\n\n", snmp.GetLastError());
			exit(1);
		}
	}

	snmp.SetObjCount(1);
	snmp.SetObjId(0, argv[3]); //3rd argument should be a valid OID, in dotted format
	snmp.SetObjType(0, OT_OCTET_STRING);
	snmp.SetObjValue(0, "", 0);

	for (int i = 1; i<= 6; i++)
	{
		retcode = snmp.SendGetNextRequest();
		if (retcode)
		{
			printf("Manager Error: %s\n\n", snmp.GetLastError());
			exit(1);
		}

		printf("Obj Id: %s\n", snmp.GetObjId(0));
		if (snmp.GetErrorStatus() == 0)
		{
			char * value;
			int len;
			snmp.GetObjValue(0, value, len);
			printf("ObjVal: %s\n\n", value);
		}
		else
		{
			printf("Error : %s\n\n", snmp.GetErrorDescription());
		}
	}

	printf("\nPress enter to continue...");
	getchar();
	exit(1);
	return 0;
}

