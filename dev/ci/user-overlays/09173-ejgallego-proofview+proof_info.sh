if [ "$CI_PULL_REQUEST" = "9173" ] || [ "$CI_BRANCH" = "proofview+proof_info" ]; then

    ltac2_CI_REF=proofview+proof_info
    ltac2_CI_GITURL=https://github.com/ejgallego/ltac2

    equations_CI_REF=proofview+proof_info
    equations_CI_GITURL=https://github.com/ejgallego/Coq-Equations

    fiat_parsers_CI_REF=proofview+proof_info
    fiat_parsers_CI_GITURL=https://github.com/ejgallego/fiat

fi
