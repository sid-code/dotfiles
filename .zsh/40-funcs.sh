# ndless
ndlessmode() {
  export NDLESS_HOME=~/code/nspire/Ndless
  export PATH="$NDLESS_HOME/ndless-sdk/toolchain/install/bin:$NDLESS_HOME/ndless-sdk/bin:${PATH}:$NDLESS_HOME/install/bin/"
  echo "ndless initialized"
}

# mathematica
mathematica() {
  export PATH="$PATH:/home/sid/asu/mathematica/inst/bin/"
  echo "mathematica in path"
}

# ros
rosinit() {
  source /opt/ros/kinetic/setup.zsh
  ros_setup_sourced=no
  ros_user_path=/home/sid/asu/research/furi/ros
  chpwd() {
    case $PWD in
      $ros_user_path)
        if [[ "$ros_setup_sourced" == "no" ]]
        then
          source $PWD/devel/setup.zsh; echo "setup script sourced"
          ros_setup_sourced=yes
        fi;;
    esac
  }

  echo "ROS shell mode initialized"
}
