let

  region = "eu-west-1"; # ireland
  accessKeyId = "default"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.small";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
      # workaround this space
      deployment.ec2.ebsInitialRootDiskSize = 20;
    };

in
{ machine = ec2;

  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };

  #resources.ec2SecurityGroups.name = "launch-wizard-1";

}
