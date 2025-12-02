open System.Runtime.InteropServices
(* suppress native pointer warnings *)
#nowarn "51"
(* suppress null pointer warnings *)
#nowarn "9"

(* float == double *)
module Aocl =
 [<DllImport(@"AOCL-LibFlame-Win-dll.dll",
             EntryPoint = "dsyevr_",
             CallingConvention = CallingConvention.Cdecl,
             CharSet = CharSet.None,
             ExactSpelling = true)>]
 extern void dsyevr(char* jobz, char* range, char* uplo,
                    int32* n,
                    float* A, int32* lda,
                    float* vl,
                    float* vu, int32* il, int32* iu,
                    float* abstol, int32* m,
                    float* W,
                    float* Z, int32* ldz, int32* isuppz,
                    float* work, int32* lwork,
                    int32* iwork, int32* liwork,
                    int32* info)

type dsyevr_params =
 { mutable jobz : char;
   mutable range : char;
   mutable uplo: char;
   mutable n: int32;
   a: float array;
   mutable lda: int32;
   mutable vl: float;
   mutable vu: float;
   mutable il: int32;
   mutable iu: int32;
   mutable abstol: float;
   mutable m: int32;
   w: float array;
   z: float array;
   mutable ldz: int32;
   isuppz: int32 array;
   work: float array;
   mutable lwork: int32;
   iwork: int32 array;
   mutable liwork: int32;
   mutable info: int32; }
 static member Default =
  {jobz = 'V'; range = 'I'; uplo = 'U'; n = 0;
   a = [||]; lda = 0;
   vl = 0.0; vu = 0.0; il = 0; iu = 0; abstol = 0.0; m = 0;
   w = [||]; z = [||];
   ldz = 0; isuppz = [||];
   work = [||]; lwork = 0;
   iwork = [||]; liwork = 0;
   info = 0;}
 member this.Dsyevr () =
  Aocl.dsyevr(&&this.jobz,&&this.range,&&this.uplo,&&this.n,
              &&this.a[0],&&this.lda,&&this.vl,&&this.vu,&&this.il,&&this.iu,&&this.abstol,&&this.m,
              &&this.w[0],&&this.z[0],&&this.ldz,&&this.isuppz[0],
              &&this.work[0],&&this.lwork,&&this.iwork[0],&&this.liwork,&&this.info)

let _ =
 let p = {
  dsyevr_params.Default with
   a = [| 1.0; 0.0; -1.0; 0.0; 0.0; 0.0; -1.0; 0.0; 1.0 |];
   n = 3; lda = 3; ldz = 3; il = 1; iu = 2; lwork = 100; liwork = 30;
   isuppz = Array.create 6 0;
   work = Array.create 100 0.0;
   iwork = Array.create 30 0;
   w = Array.create 2 0.0;
   z = Array.create 6 0.0} in
 p.Dsyevr ();

 printfn "Subroutine Complete";
 printfn "W[0]: %f : %f %f %f" p.w[0] p.z[0] p.z[1] p.z[2];
 printfn "W[1]: %f : %f %f %f" p.w[1] p.z[3] p.z[4] p.z[5];
 ()
