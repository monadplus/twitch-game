let
  region = "eu-west-1";
  accessKeyId = "default";
in
  {
    machine =
      { resources, ... }:
      {
        deployment.targetEnv = "ec2";
        deployment.ec2 = {
          inherit region;
          inherit accessKeyId;
          instanceType = "t2.nano";
          keyPair = resources.ec2KeyPairs.my-key-pair;
          securityGroups = [ resources.ec2SecurityGroups.ssh-security-group ];
        };
      };

  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };

  resources.ec2SecurityGroups.ssh-security-group = {
      inherit region;
      inherit accessKeyId;
      description = "Security group for SSH";
      rules = [ {
        fromPort = 22;
        toPort = 22;
        sourceIp = "0.0.0.0/0";
      } {
        fromPort = 8080;
        toPort = 8080; # Doesn't work -1;
        sourceIp = "0.0.0.0/0";
      }];
  };
}
