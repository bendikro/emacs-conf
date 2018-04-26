#!/bin/bash

print_usage() {
	usage="$(basename "$0") [-h] [-c | -o | -d ] -- Install Ubuntu with zfs on root

where:
    -c  create filesystem
    -o  create config
    -d  delete filesystem
    -h  show this help text
"
	echo "$usage"
}

if [ "$#" -eq "0" ]; then
	print_usage
	exit 1
fi


pool=tpool
os_base_fs=$pool/ubuntu
schroot_name=bionic2
schroot_user=bro
TMP_MOUNT_DIR=/root/chroot/ubuntu_$schroot_name

mount_dirs=(dev proc sys)


function chroot_umount {
	for dir in ${mount_dirs[@]}
	do
		umount -R $TMP_MOUNT_DIR/$dir
	done
}

function create_fs {
	zfs create -o canmount=off -o mountpoint=/ $os_base_fs
	zfs create -o canmount=noauto -o mountpoint=legacy $os_base_fs/ROOT
	zpool set bootfs=$os_base_fs/ROOT $pool
	zfs mount $os_base_fs/ROOT

	zfs create                 -o setuid=off              $os_base_fs/home
	zfs create                                            $os_base_fs/root

	zfs create -o com.sun:auto-snapshot=false -o setuid=off -o devices=off -o sync=disabled -o mountpoint=/tmp $os_base_fs/tmp
	chmod 777 $TMP_MOUNT_DIR/tmp

	zfs create -o canmount=off -o setuid=off  -o exec=off $os_base_fs/var
	zfs create -o com.sun:auto-snapshot=false -o exec=on  $os_base_fs/var/tmp
	zfs create -o com.sun:auto-snapshot=false             $os_base_fs/var/cache
	zfs create                                            $os_base_fs/var/log
	zfs create                                            $os_base_fs/var/spool
}

function delete_fs {
	zfs destroy -r $os_base_fs
}

function create_config {

sudo bash -c "cat <<EOIPFW >> /etc/schroot/chroot.d/$schroot_name.conf
[$schroot_name]
description=Ubuntu ($schroot_name)
directory=$TMP_MOUNT_DIR
root-users=$schroot_user
type=directory
users=$schroot_user
EOIPFW"
}


while getopts ":cdruioh" opt; do
	case $opt in
		o)
			create_config
			;;
		c)
			create_fs
			;;
		d)
			delete_fs
			;;
		u)
			chroot_umount
			;;
		h)
			print_usage
			;;
		\?)
			echo "Invalid option: -$OPTARG" >&2
			;;
		:)
			echo "Option -$OPTARG requires an argument." >&2
			exit 1
			;;
	esac
done
