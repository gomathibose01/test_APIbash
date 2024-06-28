#!/bin/bash

# API script that performs End-to-End process involved right from the Masking Engine LOGIN till the Masking JOB completion.
# This file contains masking API bash scripts to perform masking of the specified column in the specified table 
#
# reference - http://10.10.1.13/masking/api-client/
#           - https://maskingdocs.delphix.com/5.3.5/Delphix_Masking_APIs/API_Examples/helpers/ 


# Login and set the correct $AUTH_HEADER.
login() {
echo "* logging into the Masking Engine..."

LOGIN_RESPONSE=$(curl -s -X POST -k --data @- ${MASKING_ENGINE}/login \
   -b ~/cookies.txt -c ~/cookies.txt -H "Content-Type: application/json"<<-EOF
{
  "username": "$USERNAME",
  "password": "$PASSWORD"
}
EOF
) || die "Login failed with exit code $?"
   # check_error "$LOGIN_RESPONSE"
     TOKEN=$(echo $LOGIN_RESPONSE | tr -d '"' | tr -d "{" | tr -d "}")
     AUTH_HEADER=$TOKEN
    echo $AUTH_HEADER 'AUTH HEADER OBTAINED...'
}

# GET ENVIRONMENT by ENVIRONMENT NAME.
get_delete_environment() {
  
echo "* check for the environment existence $ENVIRONMENT_NAME"
ENVIRONMENT_GET_RESPONSE=$(curl -s -X GET  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/environments/<<-EOF
EOF
) || die "Get Environment failed with exit code $?"

if [ $(echo $ENVIRONMENT_GET_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred during GET operation : $ENVIRONMENT_GET_RESPONSE"
    exit 1
else
    ENVIRONMENT_ID=$(echo "$ENVIRONMENT_GET_RESPONSE" |  grep -o '"environmentId":[^,]*,"environmentName":"'"$ENVIRONMENT_NAME"'"' |grep -o '"environmentId"[^,]*'| grep -o '[^:]*$' | tr -d '"' )
    if [ $ENVIRONMENT_ID > ' ' ]; then
        echo 'Environment already exists with ID '$ENVIRONMENT_ID  
        echo '* Deleting the existing environment.... '
        ENVIRONMENT_DELETE_RESPONSE=$(curl -s -X DELETE -H ${AUTH_HEADER} -H 'accept: */*' \
        -k --data @- ${MASKING_ENGINE}/environments/${ENVIRONMENT_ID}<<-EOF
EOF
    ) || die "Delete Environment failed with exit code $?"
        #echo 'ENVIRONMENT_DELETE_RESPONSE=' $ENVIRONMENT_DELETE_RESPONSE
        if [ $(echo $ENVIRONMENT_DELETE_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
            echo "Error occurred during deletion : $ENVIRONMENT_DELETE_RESPONSE "
            exit 1
        fi
    fi
fi
}

# GET APPLICATION by APPLICATION NAME.
get_delete_application() {
   
echo "* check for the application existence $APPLICATION_NAME"
APPLICATION_GET_RESPONSE=$(curl -s -X GET  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/applications/<<-EOF
EOF
) || die "Get Application failed with exit code $?"

if [ $(echo $APPLICATION_GET_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred during GET operation : $APPLICATION_GET_RESPONSE "
    exit 1
else
    APPLICATION_ID=$(echo "$APPLICATION_GET_RESPONSE" |  grep -o '"applicationId":[^,]*,"applicationName":"'"$APPLICATION_NAME"'"' |grep -o '"applicationId"[^,]*'| grep -o '[^:]*$' | tr -d '"' )
    if [ $APPLICATION_ID > ' ' ]; then
        echo 'Application already exists with ID '$APPLICATION_ID  
        echo '* Deleting the existing application.... '
        APPLICATION_DELETE_RESPONSE=$(curl -X 'DELETE' \
  http://10.10.1.13/masking/api/v5.1.27/applications/"$APPLICATION_ID" \
  -H 'accept: */*' \
   -H ${AUTH_HEADER}<<-EOF
EOF
        ) || die "Delete Application failed with exit code $?"
        #echo 'APPLICATION_DELETE_RESPONSE=' $APPLICATION_DELETE_RESPONSE
        if [ $(echo $APPLICATION_DELETE_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
            echo "Error occurred during deletion : $APPLICATION_DELETE_RESPONSE "
            exit 1
        fi
    fi
fi
}

# create a new application  applicationName in $APPLICATION_ID.
create_application() {

echo "* creating applications $APPLICATION_NAME"
APPLICATION_CR8_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/applications<<-EOF
{
    "applicationName": "$APPLICATION_NAME"
}  
EOF
) || die "Create Application failed with exit code $?"
#echo 'APPLICATION_CR8_RESPONSE is' $APPLICATION_CR8_RESPONSE
if [ $(echo $APPLICATION_CR8_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $APPLICATION_CR8_RESPONSE"
    exit 1
fi
APPLICATION_ID=$(echo $APPLICATION_CR8_RESPONSE | grep -Eo '"applicationId"[^,]*'| grep -Eo '[^:]*$')
echo 'APPLICATION ID is ' $APPLICATION_ID
}

# create a new environment
create_environment() {
echo "* creating environment $ENVIRONMENT_NAME"
ENV_CR8_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/environments<<-EOF
{
  "environmentName": "$ENVIRONMENT_NAME",
  "applicationId": "$APPLICATION_ID",
  "purpose": "MASK"
}  
EOF
) || die "Create Environment failed with exit code $?"
#echo ENV_CR8_RESPONSE is $ENV_CR8_RESPONSE
if [ $(echo $ENV_CR8_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $ENV_CR8_RESPONSE"
    exit 1
fi
ENVIRONMENT_ID=$(echo $ENV_CR8_RESPONSE | grep -Eo '"environmentId"[^,]*'| grep -Eo '[^:]*$')
echo 'ENVIRONMENT_ID ID is ' $ENVIRONMENT_ID

}

# Test a new connection
test_connection() {
echo "* creating connection $CONNECTION_NAME"


CONNECTION_CR8_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/database-connectors/test<<-EOF
{
  "connectorName": "$CONNECTION_NAME",
  "databaseType": "$DATABASE_TYPE",
  "environmentId": $ENVIRONMENT_ID,
  "host": "$HOST_IP",
  "password": "$HOST_PASSWORD",
  "port": $PORT,
  "schemaName": "$DBSCHEMA",
  "databaseName": "$DBNAME",
  "username": "$HOST_USERNAME"
  }  
EOF
) || die "test Connector failed with exit code $?"
#echo CONNECTION_CR8_RESPONSE is $CONNECTION_CR8_RESPONSE
if [ $(echo $CONNECTION_CR8_RESPONSE | grep  '"Connection Succeeded"' |wc -l) -gt 0 ]; then
 echo echo "creating connection..." 

else 
     echo "Test connection failed"
     die "Create Connector failed with exit code $?" 
fi
}

# Create a new connection
create_connection() {
echo "* creating connection $CONNECTION_NAME"

CONNECTION_CR8_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/database-connectors<<-EOF
{
  "connectorName": "$CONNECTION_NAME",
  "databaseType": "$DATABASE_TYPE",
  "environmentId": $ENVIRONMENT_ID,
  "host": "$HOST_IP",
  "password": "$HOST_PASSWORD",
  "port": $PORT,
  "schemaName": "$DBSCHEMA",
  "databaseName": "$DBNAME",
  "username": "$HOST_USERNAME"
  }  
EOF
) || die "Create Connector failed with exit code $?"
#echo CONNECTION_CR8_RESPONSE is $CONNECTION_CR8_RESPONSE
if [ $(echo $CONNECTION_CR8_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $CONNECTION_CR8_RESPONSE"
    exit 1
fi
CONNECTION_ID=$(echo $CONNECTION_CR8_RESPONSE | grep -Eo '"databaseConnectorId"[^,]*'| grep -Eo '[^:]*$')
echo ' CONNECTION_ID is ' $CONNECTION_ID
}

#fetch all accessible tables
fetch_tables() {
echo "* Fetching Tables under the new connection $CONNECTION_NAME $CONNECTION_ID" 

FETCH_RESPONSE=$(curl -s -X GET  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/database-connectors/${CONNECTION_ID}/fetch<<-EOF
EOF
) || die "Fetch Tables under Connector failed with exit code $?"
#echo FETCH_RESPONSE is $FETCH_RESPONSE
if [ $(echo $FETCH_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $FETCH_RESPONSE"
    exit 1
fi
#check whether the Tables exists in the environment 
if [ $(echo ${FETCH_RESPONSE}| grep  ${INPUT_TABLE_NAME1} |wc -l) -gt 0 ]; then
 echo  "Table available"
else echo "Table $INPUT_TABLE_NAME is NOT AVAILABLE in the environment $HOST_IP; Kindly create the Table & re-run"
exit 1
fi

}
create_databaseruleset() {
echo "* creating database rulset for the connector $CONNECTION_NAME $CONNECTION_ID" 
RS_CR8_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/database-rulesets<<-EOF
{
  "rulesetName": "$RS_NAME",
  "databaseConnectorId": $CONNECTION_ID
}  
EOF
) || die "Create Ruleset failed with exit code $?"
#echo RS_CR8_RESPONSE is $RS_CR8_RESPONSE
if [ $(echo $RS_CR8_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $RS_CR8_RESPONSE"
    exit 1
fi
RS_ID=$(echo $RS_CR8_RESPONSE | grep -Eo '"databaseRulesetId"[^,]*'| grep -Eo '[^:]*$')
echo 'Rulset ID is ' $RS_ID for $RS_NAME
}

create_tbl_metadata() {
echo "* Selecting Tables under rulset for the Rulset $RS_NAME and ID $RS_ID" 
CR8_TBLMETA_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/table-metadata<<-EOF
    {
      "tableName": "$INPUT_TABLE_NAME1",
      "rulesetId": $RS_ID
    }
EOF
) || die "Create TableMetdata failed with exit code $?"
#echo CR8_TBLMETA_RESPONSE is $CR8_TBLMETA_RESPONSE
if [ $(echo $CR8_TBLMETA_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $CR8_TBLMETA_RESPONSE"
    exit 1
fi
TABLE_METADATA_ID=$(echo $CR8_TBLMETA_RESPONSE | grep -Eo '"tableMetadataId"[^,]*'| grep -Eo '[^:]*$')
echo "using table metadata '$TABLE_METADATA_ID'"
}

get_column_metadata() {
echo "* Get Column Metadata under the rulset for the Rulset $RS_NAME ,  ID $RS_ID  , Table Metadata  $TABLE_METADATA_ID"
GET_COLMETA_RESPONSE=$(curl -s -X GET  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/column-metadata?table_metadata_id=${TABLE_METADATA_ID}<<-EOF
EOF
) || die "Create TableMetdata failed with exit code $?"
#echo GET_COLMETA_RESPONSE is $GET_COLMETA_RESPONSE
#COLUMN_METADATA_ID=$(echo $GET_COLMETA_RESPONSE | grep -Eo '"columnMetadataId"[^,]*'| grep -Eo '[^:]*$')
#echo "Column Metadata ID: '$COLUMN_METADATA_ID'"
if [ $(echo $GET_COLMETA_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $GET_COLMETA_RESPONSE"
    exit 1
fi
COLUMN_METADATA_ID_TOUSE=$(echo "$GET_COLMETA_RESPONSE" | grep -o '"columnMetadataId":[^,]*,"columnName":"'"$COLNAME"'"' |grep -o '"columnMetadataId"[^,]*'| grep -o '[^:]*$' | tr -d '"' )
echo 'COLUMN_METADATA_ID_TOUSE :' $COLUMN_METADATA_ID_TOUSE
}

create_inventory() {
#COLUMN_METADATA_ID_TOUSE=1493  #TEST##########
echo "* creating inventory under the rulset for the Rulset $RS_NAME;ID $RS_ID;Table Metadata $TABLE_METADATA_ID;Column: $COLUMN_METDATA_ID_TOUSE"
CR8_INV_RESPONSE=$(curl -s -X PUT  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/column-metadata/$COLUMN_METADATA_ID_TOUSE<<-EOF
{
    "algorithmName": "${ALGORITHM1}",
    "domainName": "${DOMAIN1}",
    "isProfilerWritable": false 
}
EOF
) || die "Create Inventory failed with exit code $?"
if [ $(echo $CR8_INV_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $CR8_INV_RESPONSE"
    exit 1
fi
#echo CR8_INV_RESPONSE is $CR8_INV_RESPONSE
}

#create masking job
create_maskingjob() {
echo "* creating Masking Job for the provided Rulset $RS_ID $RS_NAME..."

CR8_MASKJOB_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json'  -H 'accept: application/json' \
    -k --data  @- ${MASKING_ENGINE}/masking-jobs<<-EOF
{
    "jobName": "${JOB_NAME}", 
    "rulesetId":${RS_ID}, 
    "jobDescription": "Demo Job run", 
    "feedbackSize": 10000,
    "onTheFlyMasking": false 
}  
EOF
) || die "Create Masking Job failed with exit code $?"
#echo CR8_MASKJOB_RESPONSE is $CR8_MASKJOB_RESPONSE
if [ $(echo $CR8_MASKJOB_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $CR8_MASKJOB_RESPONSE"
    exit 1
fi
MASKING_JOB_ID=$(echo $CR8_MASKJOB_RESPONSE | grep -Eo '"maskingJobId"[^,]*'| grep -Eo '[^:]*$')
echo "Masking Job ID: '$MASKING_JOB_ID'"
}

# Run the masking job
run_maskingjob() {

echo "* Run the Masking Job for the provided masking Job ID $MASKING_JOB_ID..."
RUN_MASKINGJOB_RESPONSE=$(curl -s -X POST  -H ${AUTH_HEADER} -H 'Content-Type: application/json'  -H 'accept: application/json' \
    -k --data  @- ${MASKING_ENGINE}/executions<<-EOF
{
     "jobId": "$MASKING_JOB_ID"
}
EOF
) || die " Masking Job execution failed with exit code $?"
#echo RUN_MASKINGJOB_RESPONSE is $RUN_MASKINGJOB_RESPONSE
if [ $(echo $RUN_MASKINGJOB_RESPONSE | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $RUN_MASKINGJOB_RESPONSE"
    exit 1
fi
MASKING_EXEC_ID=$(echo $RUN_MASKINGJOB_RESPONSE | grep -Eo '"executionId"[^,]*'| grep -Eo '[^:]*$')
echo "MaskingJob execution ID: '$MASKING_EXEC_ID'"
}

checkexecutionstatus() {
echo "* Check Execution status of  the Masking Job ID $MASKING_JOB_ID..."
JOBSTATUS=$(echo $RUN_MASKINGJOB_RESPONSE | grep -Eo '"status"[^,]*'| grep -Eo '[^:]*$')
echo "MaskingJob execution status: $JOBSTATUS"
while [ ${JOBSTATUS} == '"RUNNING"' ]
do
STATUS=$(curl -s -X GET  -H ${AUTH_HEADER} -H 'Content-Type: application/json' \
    -k --data  @- ${MASKING_ENGINE}/executions/$MASKING_EXEC_ID<<-EOF
EOF
)|| die "Masking Execution status check failed with exit code $?"
if [ $(echo $STATUS | grep  "errorMessage" |wc -l) -gt 0 ]; then
    echo "Error occurred  : $STATUS"
    exit 1
fi
JOBSTATUS=$(echo $STATUS | grep -Eo '"status"[^,]*'| grep -Eo '[^:]*$')
echo "Current status as of" $(date) " : "  ${JOBSTATUS}
printf "."
sleep 10
done
printf "\n"
if [ ${JOBSTATUS} != '"SUCCEEDED"' ]; then
echo "Job Error: $JOBSTATUS ... $STATUS"
else
echo "Masking Job Completed: $JOBSTATUS"
echo ${STATUS} 
fi
}

# Print the message and exit the program.
die() {
    echo "*******************************************************************************"
    echo "$(basename $0) ERROR: $*" >&2
    echo "*******************************************************************************"
    exit 1
}

# call the below Masking operations via API calls
login
get_delete_environment
get_delete_application
create_application 
create_environment
test_connection
create_connection
fetch_tables
create_databaseruleset
create_tbl_metadata
get_column_metadata
create_inventory
create_maskingjob
run_maskingjob
checkexecutionstatus
echo 'Application ID : ' ${APPLICATION_ID}
echo 'Environment ID : ' ${ENVIRONMENT_ID}

