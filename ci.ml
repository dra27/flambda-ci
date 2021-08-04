
let program_name = "flambda-ci"

open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let () = Prometheus_unix.Logging.init ()

(* Link for GitHub statuses. *)
let url = Uri.of_string "http://ubuntu.thor.dra27.uk:8080"

let root_spec ~base cont =
  (* XXX The computation of the base sha needs tweaking:
         - We pull the container once per week
         - If the version is unchanged, then we use the _previous_ sha256 for the container
         - ocaml-flambda/dune#special_dune should be an input to this process (if Dune changes, we definitely pull a new sha)
   *)
  let from = Docker.Image.hash base in
  let network = ["host"] in
  let open Obuilder_spec in
  let spec =
    user ~uid:1000 ~gid:1000 ::
    workdir "/src" ::
    run ~network "sudo apt-get install -yy autoconf parallel" ::
    (* XXX This part should be using a sha for the pinning *)
    run ~network "opam pin add dune git+https://github.com/ocaml-flambda/dune.git#special_dune" ::
    copy ["."] ~dst:"/src/" ::
    run "autoconf" ::
    cont ~network
  in
  stage ~from spec

let base_spec ~base =
  root_spec ~base (fun ~network:_ -> [])
  |> Fmt.to_to_string Obuilder_spec.pp

let build_and_test_spec ~base ~middle =
  root_spec ~base (fun ~network:_ ->
    let open Obuilder_spec in [
      run "./configure --prefix=/src/_install --enable-middle-end=%s --with-dune=$(opam exec -- which dune) && make -j ci" middle;
    ])
  |> Fmt.to_to_string Obuilder_spec.pp

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let github_status_of_state = function
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let build ~cluster ~docker_base ~head =
  let src = Current.map (fun head -> [Github.Api.Commit.id head]) head in
  let base_spec =
    let+ base = docker_base in
    { Cluster_api.Obuilder_job.Spec.spec = `Contents (base_spec ~base) }
  in
  let build_and_test ~base ~middle =
    let+ base = docker_base
    and+ () = base in
    { Cluster_api.Obuilder_job.Spec.spec = `Contents (build_and_test_spec ~base ~middle) }
  in
  let base = Current_ocluster.build_obuilder cluster ~cache_hint:"flambda2-ci" ~src ~pool:"linux-x86_64" base_spec in
  let clambda =
    Current_ocluster.build_obuilder cluster ~cache_hint:"flambda2-ci" ~src ~pool:"linux-x86_64" (build_and_test ~base ~middle:"closure")
  and flambda =
    Current_ocluster.build_obuilder cluster ~cache_hint:"flambda2-ci" ~src ~pool:"linux-x86_64" (build_and_test ~base ~middle:"flambda") in
  Current.pair clambda flambda
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocurrent"

let pipeline ~github ~cluster ~repo () : unit Current.t =
  (*let head = Github.Api.head_commit github repo in*)
  let docker_base = Docker.pull ~schedule:weekly "ocaml/opam:ubuntu-20.04-ocaml-4.11" in
  Github.Api.ci_refs github repo
  |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
    build ~cluster ~docker_base ~head

let main config mode github submission_uri repo =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let connection = Current_ocluster.Connection.create submission_cap in
  let cluster = Current_ocluster.v connection in
  let engine = Current.Engine.create ~config (pipeline ~github ~cluster ~repo) in
  let routes =
    Routes.(s "webhooks" / s "github" /? nil @--> Github.webhook) ::
    Current_web.routes engine
  in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.pos 0 (Arg.some Github.Repo_id.cmdliner) None @@
  Arg.info
    ~doc:"The GitHub repository (owner/name) to monitor."
    ~docv:"REPO"
    []

let submission_service =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let cmd =
  let doc = "Monitor a GitHub repository." in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.Api.cmdliner $ submission_service $ repo)),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
