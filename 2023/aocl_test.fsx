open System.Runtime.InteropServices
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

(*
int main (int argc, char **argv)
{
 double A[] = {1.0, 0.0, -1.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0};
 const char *param = "VIU";
 const int32_t n = 3;
 const int32_t lda = 3;
 const int32_t ldz = 3;
 const int32_t il = 1;
 const int32_t iu = 2;
 const double abstol = 0;
 const int32_t lwork = 100;
 const int32_t liwork = 30;
 int32_t m=0, info=0;
 int32_t isuppz[6];
 double work[100];
 int32_t iwork[30];
 double W[2], Z[6];
 
 dsyevr_(&param[0],&param[1],&param[2],&n, A, &lda, NULL, NULL, &il, &iu, &abstol, &m, W, Z, &ldz, isuppz, work, &lwork, iwork, &liwork, &info);
 printf("Subroutine Complete\n");
 // printf("INFO: %i, LWORK: %lf, LIWORK: %i\n", info, work[0], iwork[0]);
 printf("W[0]: %f : %f %f %f\n", W[0], Z[0], Z[1], Z[2]);
 printf("W[1]: %f : %f %f %f\n", W[1], Z[3], Z[4], Z[5]);
 return 0;
}
*)
(* suppress native pointer warnings *)
#nowarn "51"
(* suppress null pointer warnings *)
#nowarn "9"

let _ =
 let mutable a_mat = [| 1.0; 0.0; -1.0; 0.0; 0.0; 0.0; -1.0; 0.0; 1.0 |] in
 let param = "VIU" |> Seq.toArray in
 let mutable n = 3 in
 let mutable lda = 3 in
 let mutable ldz = 3 in
 let mutable il = 1 in
 let mutable iu = 2 in
 let mutable abstol = 0.0 in
 let mutable lwork = 100 in
 let mutable liwork = 30 in
 let mutable m = 0 in
 let mutable info = 0 in
 let isuppz = Array.create 6 0 in
 let work = Array.create 100 0.0 in
 let iwork = Array.create 30 0 in
 let w = Array.create 2 0.0 in
 let z = Array.create 6 0.0 in

 Aocl.dsyevr(&&param[0],
             &&param[1],
             &&param[2],
             &&n, &&a_mat[0], &&lda,
             NativeInterop.NativePtr.nullPtr, NativeInterop.NativePtr.nullPtr,
             &&il, &&iu, &&abstol, &&m, &&w[0], &&z[0], &&ldz, &&isuppz[0], &&work[0], &&lwork, &&iwork[0], &&liwork, &&info);
 printfn "Subroutine Complete";
 printfn "W[0]: %f : %f %f %f" w[0] z[0] z[1] z[2];
 printfn "W[1]: %f : %f %f %f" w[1] z[3] z[4] z[5];
 ()
 
