#!/bin/bash

# API script that performs deletion of the Environment and Application from the Masking Engine.
# This can be run in case of backout process required on the card api masking.
# update the application name and environment name variables here in this script.
#

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

# main/start process

# call the below Masking operations via API calls
login
get_delete_environment
get_delete_application

