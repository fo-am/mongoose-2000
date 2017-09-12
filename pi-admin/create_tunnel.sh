#!/bin/bash
create_ssh_tunnel() {
    /usr/bin/ssh -o "ExitOnForwardFailure yes" -N -R 2222:localhost:22 mongoose@be.fo.am &
    if [[ $? -eq 0 ]]; then
	echo SSH tunnel created successfully
    else
	echo An error occurred creating SSH tunnel. RC was $?
    fi
}
create_rabbit_tunnel() {
    /usr/bin/ssh -o "ExitOnForwardFailure yes" -N -R 5672:localhost:5672 mongoose@be.fo.am &
    if [[ $? -eq 0 ]]; then
	echo Rabbit tunnel created successfully
    else
	echo An error occurred creating a RabbitMQ tunnel. RC was $?
    fi
}

/bin/pidof ssh
if [[ $? -ne 0 ]]; then
    echo Creating new ssh tunnel connection
    create_ssh_tunnel
    echo Creating new RabbitMQ tunnel connection
    create_rabbit_tunnel
fi
